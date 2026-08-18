using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml.Serialization;

namespace XgkHmiDesigner
{
    [Serializable]
    public class HmiProject
    {
        public string PlcIp = "192.168.1.120";
        public int Port = 2004;
        public int CycleMs = 300;
        public List<HmiItem> Items = new List<HmiItem>();
    }

    [Serializable]
    public class HmiItem
    {
        public string Id = Guid.NewGuid().ToString("N");
        public bool Enabled = true;
        public string Type = "SWITCH";       // SWITCH, LAMP, NUM_INPUT, NUM_DISPLAY, TEXT
        public string Name = "새 스위치";
        public string Device = "M1000";      // SWITCH/LAMP=P/M bit, NUM*=D word
        public string MonitorDevice = "";    // SWITCH 상태와 별도로 확인할 P/M bit
        public string Action = "토글";       // 토글, ON, OFF, 순간, ON/OFF
        public int Min = 0;
        public int Max = 65535;
        public int X = 20;
        public int Y = 20;
        public int Width = 180;
        public int Height = 100;
    }

    public class XgtProtocolException : Exception
    {
        public int ErrorCode;
        public XgtProtocolException(string message, int code) : base(message) { ErrorCode = code; }
    }

    public class XgtClient : IDisposable
    {
        private class HeaderProfile
        {
            public string Company;
            public byte CpuInfo;
            public byte Position;
            public bool UseBcc;
            public string Name;

            public HeaderProfile(string company, byte cpuInfo, byte position, bool useBcc, string name)
            {
                Company = company; CpuInfo = cpuInfo; Position = position; UseBcc = useBcc; Name = name;
            }
        }

        private TcpClient _tcp;
        private NetworkStream _stream;
        private ushort _invokeId = 1;
        private readonly string _ip;
        private readonly int _port;
        private readonly int _timeoutMs;
        private HeaderProfile _profile;
        private string _negotiationLog = "";

        public string ProfileName { get { return _profile == null ? "미확정" : _profile.Name; } }
        public string NegotiationLog { get { return _negotiationLog; } }
        public bool Connected { get { return _tcp != null && _tcp.Connected && _stream != null; } }

        public XgtClient(string ip, int port, int timeoutMs)
        {
            _ip = ip;
            _port = port;
            _timeoutMs = timeoutMs;
        }

        private void ConnectSocket()
        {
            DisposeSocket();
            _tcp = new TcpClient();
            IAsyncResult ar = _tcp.BeginConnect(_ip, _port, null, null);
            if (!ar.AsyncWaitHandle.WaitOne(_timeoutMs))
            {
                try { _tcp.Close(); } catch { }
                throw new TimeoutException("PLC TCP 연결 시간 초과");
            }
            _tcp.EndConnect(ar);
            _tcp.NoDelay = true;
            _stream = _tcp.GetStream();
            _stream.ReadTimeout = _timeoutMs;
            _stream.WriteTimeout = _timeoutMs;
            _invokeId = 1;
        }

        public void Connect()
        {
            // r004 프로젝트: XBM-DN32H2(XGB/MK), 내장 FEnet이 Base 0 / Slot 1로 저장되어 있음.
            // 펌웨어/헤더 처리 차이를 고려해 공식 헤더 조합을 새 TCP 연결마다 자동 시험한다.
            HeaderProfile[] candidates = new HeaderProfile[] {
                new HeaderProfile("LSIS-XGT",   0xB0, 0x01, true,  "XGB(MK) / Slot1 / BCC"),
                new HeaderProfile("LSIS-XGT",   0xB0, 0x00, true,  "XGB(MK) / Slot0 / BCC"),
                new HeaderProfile("LSIS-XGT",   0xB0, 0x01, false, "XGB(MK) / Slot1 / BCC=00"),
                new HeaderProfile("LSIS-XGT",   0xB0, 0x00, false, "XGB(MK) / Slot0 / BCC=00"),
                new HeaderProfile("LGIS-GLOFA", 0xB0, 0x01, true,  "XGB(MK) / GLOFA / Slot1 / BCC"),
                new HeaderProfile("LGIS-GLOFA", 0xB0, 0x00, true,  "XGB(MK) / GLOFA / Slot0 / BCC"),
                new HeaderProfile("LGIS-GLOFA", 0xB0, 0x01, false, "XGB(MK) / GLOFA / Slot1 / BCC=00"),
                new HeaderProfile("LGIS-GLOFA", 0xB0, 0x00, false, "XGB(MK) / GLOFA / Slot0 / BCC=00")
            };

            StringBuilder log = new StringBuilder();
            Exception last = null;
            for (int i = 0; i < candidates.Length; i++)
            {
                try
                {
                    _profile = candidates[i];
                    ConnectSocket();
                    ProbeKnownRead();
                    log.AppendLine("OK  " + _profile.Name);
                    _negotiationLog = log.ToString();
                    return;
                }
                catch (Exception ex)
                {
                    last = ex;
                    log.AppendLine("FAIL " + candidates[i].Name + " : " + ex.Message);
                    DisposeSocket();
                }
            }
            _profile = null;
            _negotiationLog = log.ToString();
            throw new IOException("XGT 자동 판별 실패. 통신 로그의 TX/RX 내용을 확인하십시오." + (last != null ? " 마지막 오류: " + last.Message : ""));
        }

        private static byte[] UInt16LE(int value)
        {
            return new byte[] { (byte)(value & 0xFF), (byte)((value >> 8) & 0xFF) };
        }

        private static int ReadUInt16LE(byte[] data, int offset)
        {
            return data[offset] | (data[offset + 1] << 8);
        }

        private static string Hex(byte[] data)
        {
            if (data == null) return "";
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < data.Length; i++)
            {
                if (i > 0) sb.Append(' ');
                sb.Append(data[i].ToString("X2"));
            }
            return sb.ToString();
        }

        private byte[] BuildHeader(int bodyLength)
        {
            if (_profile == null) throw new InvalidOperationException("XGT 헤더 프로필이 선택되지 않았습니다.");
            byte[] header = new byte[20];
            byte[] company;
            if (_profile.Company == "LGIS-GLOFA")
                company = Encoding.ASCII.GetBytes("LGIS-GLOFA");
            else
                company = Encoding.ASCII.GetBytes("LSIS-XGT\0\0");
            Buffer.BlockCopy(company, 0, header, 0, 10);
            header[10] = 0x00; // PLC Info: Client -> Server don't care
            header[11] = 0x00;
            header[12] = _profile.CpuInfo;
            header[13] = 0x33; // Client -> Server
            header[14] = (byte)(_invokeId & 0xFF);
            header[15] = (byte)((_invokeId >> 8) & 0xFF);
            header[16] = (byte)(bodyLength & 0xFF);
            header[17] = (byte)((bodyLength >> 8) & 0xFF);
            header[18] = _profile.Position;
            if (_profile.UseBcc)
            {
                int sum = 0;
                for (int i = 0; i < 19; i++) sum += header[i];
                header[19] = (byte)(sum & 0xFF);
            }
            else
            {
                header[19] = 0x00;
            }
            _invokeId++;
            return header;
        }

        private byte[] ReadExact(int count)
        {
            byte[] data = new byte[count];
            int offset = 0;
            while (offset < count)
            {
                int n = _stream.Read(data, offset, count - offset);
                if (n <= 0) throw new IOException("PLC가 TCP 연결을 종료했습니다.");
                offset += n;
            }
            return data;
        }

        private byte[] Exchange(byte[] bodyBytes)
        {
            if (_stream == null) throw new InvalidOperationException("PLC에 연결되지 않았습니다.");
            byte[] header = BuildHeader(bodyBytes.Length);
            byte[] tx = new byte[header.Length + bodyBytes.Length];
            Buffer.BlockCopy(header, 0, tx, 0, header.Length);
            Buffer.BlockCopy(bodyBytes, 0, tx, header.Length, bodyBytes.Length);
            _stream.Write(tx, 0, tx.Length);
            _stream.Flush();

            byte[] rh = ReadExact(20);
            string company8 = Encoding.ASCII.GetString(rh, 0, 8);
            string company10 = Encoding.ASCII.GetString(rh, 0, 10).TrimEnd('\0', ' ');
            bool companyOk = company8 == "LSIS-XGT" || company10 == "LGIS-GLOFA";
            if (!companyOk)
                throw new IOException("XGT 헤더 ID 불일치. TX=[" + Hex(tx) + "] RXH=[" + Hex(rh) + "]");
            if (rh[13] != 0x11)
                throw new IOException("XGT 응답 방향 오류 0x" + rh[13].ToString("X2") + ". TX=[" + Hex(tx) + "] RXH=[" + Hex(rh) + "]");

            int responseLength = ReadUInt16LE(rh, 16);
            if (responseLength <= 0 || responseLength > 4096)
                throw new IOException("XGT 응답 Length=" + responseLength + ". TX=[" + Hex(tx) + "] RXH=[" + Hex(rh) + "]");

            byte[] rb = ReadExact(responseLength);
            return rb;
        }

        private void ProbeKnownRead()
        {
            // XGB FEnet 구버전 매뉴얼에도 있는 가장 기본적인 프레임 그대로 사용:
            // Read Individual / WORD / 1 block / %MW0
            byte[] body = new byte[] {
                0x54,0x00, 0x02,0x00, 0x00,0x00, 0x01,0x00,
                0x04,0x00, 0x25,0x4D,0x57,0x30
            };
            byte[] rb = Exchange(body);
            if (rb.Length < 10) throw new IOException("%MW0 시험 읽기 응답 데이터 부족: " + rb.Length);
            int cmd = ReadUInt16LE(rb, 0);
            if (cmd != 0x0055) throw new IOException("%MW0 시험 읽기 응답 명령 오류: 0x" + cmd.ToString("X4"));
            int error = ReadUInt16LE(rb, 6);
            if (error != 0)
            {
                int detail = rb.Length >= 10 ? ReadUInt16LE(rb, 8) : error;
                throw new XgtProtocolException("%MW0 시험 읽기 오류 0x" + detail.ToString("X4"), detail);
            }
            int blocks = ReadUInt16LE(rb, 8);
            if (blocks < 1) throw new IOException("%MW0 시험 읽기 블록 수가 0입니다.");
        }

        private static void ParseBitAddress(string address, out char area, out int word, out int bit)
        {
            string a = address.Trim().ToUpperInvariant();
            if (a.Length < 2)
                throw new ArgumentException("지원하지 않는 BIT 주소: " + address);

            area = a[0];
            if (area != 'P' && area != 'M')
                throw new ArgumentException("지원하는 BIT 영역은 P/M 입니다: " + address);

            string raw = a.Substring(1);

            // XGB의 P/M 비트 표기 규칙:
            // 첫 글자는 디바이스 종류, 중간 자리는 10진수 WORD 위치, 마지막 한 자리는 16진수 비트 위치(0~F).
            // 예) M01008 = MW100의 bit 8 = XGT 직접변수 %MX1608
            //     M0100F = MW100의 bit F = XGT 직접변수 %MX1615
            //     P00120 = PW12의 bit 0 = XGT 직접변수 %PX192
            if (raw.Length < 2)
                throw new ArgumentException(area + " BIT 주소가 잘못되었습니다: " + address);

            char bitChar = raw[raw.Length - 1];
            if (bitChar >= '0' && bitChar <= '9') bit = bitChar - '0';
            else if (bitChar >= 'A' && bitChar <= 'F') bit = 10 + (bitChar - 'A');
            else throw new ArgumentException("P BIT 주소의 마지막 자리가 잘못되었습니다: " + address);

            string wordText = raw.Substring(0, raw.Length - 1);
            if (!Int32.TryParse(wordText, out word))
                throw new ArgumentException("P BIT 주소의 WORD 부분이 잘못되었습니다: " + address);
        }

        private static string ToXgtBitAddress(string address)
        {
            char area;
            int word, bit;
            ParseBitAddress(address, out area, out word, out bit);
            return "%" + area + "X" + checked(word * 16 + bit).ToString();
        }

        private Dictionary<int, ushort> ReadAreaWords(char area, IList<int> words)
        {
            if (words == null || words.Count == 0) return new Dictionary<int, ushort>();
            if (words.Count > 16) throw new ArgumentException("한 프레임에서 최대 16 WORD를 읽습니다.");
            area = Char.ToUpperInvariant(area);
            if (area != 'P' && area != 'M' && area != 'D')
                throw new ArgumentException("지원하지 않는 WORD 영역: " + area);

            MemoryStream body = new MemoryStream();
            body.WriteByte(0x54); body.WriteByte(0x00);
            body.WriteByte(0x02); body.WriteByte(0x00);
            body.WriteByte(0x00); body.WriteByte(0x00);
            byte[] bc = UInt16LE(words.Count);
            body.Write(bc, 0, bc.Length);

            for (int i = 0; i < words.Count; i++)
            {
                string xgt = "%" + area + "W" + words[i].ToString();
                byte[] name = Encoding.ASCII.GetBytes(xgt);
                byte[] len = UInt16LE(name.Length);
                body.Write(len, 0, len.Length);
                body.Write(name, 0, name.Length);
            }

            byte[] rb = Exchange(body.ToArray());
            if (rb.Length < 10) throw new IOException("XGT WORD 읽기 응답이 짧습니다: " + rb.Length);
            int command = ReadUInt16LE(rb, 0);
            if (command != 0x0055) throw new IOException("XGT 읽기 응답 명령 오류: 0x" + command.ToString("X4"));
            int error = ReadUInt16LE(rb, 6);
            if (error != 0)
            {
                int detail = rb.Length >= 10 ? ReadUInt16LE(rb, 8) : error;
                throw new XgtProtocolException("PLC XGT 읽기 오류 0x" + detail.ToString("X4") + " (ErrorStatus=0x" + error.ToString("X4") + ")", detail);
            }

            int blockCount = ReadUInt16LE(rb, 8);
            int pos = 10;
            Dictionary<int, ushort> result = new Dictionary<int, ushort>();
            int count = Math.Min(blockCount, words.Count);

            for (int i = 0; i < count; i++)
            {
                if (pos + 2 > rb.Length) throw new IOException("WORD 응답 길이 부족");
                int dataLen = ReadUInt16LE(rb, pos); pos += 2;
                if (dataLen < 1 || pos + dataLen > rb.Length) throw new IOException("WORD 응답 블록 길이 오류: " + dataLen);

                ushort value = rb[pos];
                if (dataLen >= 2) value = (ushort)(rb[pos] | (rb[pos + 1] << 8));
                result[words[i]] = value;
                pos += dataLen;
            }
            return result;
        }

        public Dictionary<string, bool> ReadBits(IList<string> addresses)
        {
            if (_stream == null) throw new InvalidOperationException("PLC에 연결되지 않았습니다.");
            Dictionary<string, bool> result = new Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase);
            if (addresses == null || addresses.Count == 0) return result;

            Dictionary<char, List<int>> areaWords = new Dictionary<char, List<int>>();
            for (int i = 0; i < addresses.Count; i++)
            {
                char area;
                int word, bit;
                ParseBitAddress(addresses[i], out area, out word, out bit);
                if (!areaWords.ContainsKey(area)) areaWords[area] = new List<int>();
                if (!areaWords[area].Contains(word)) areaWords[area].Add(word);
            }

            Dictionary<char, Dictionary<int, ushort>> valuesByArea = new Dictionary<char, Dictionary<int, ushort>>();
            foreach (KeyValuePair<char, List<int>> kv in areaWords)
                valuesByArea[kv.Key] = ReadAreaWords(kv.Key, kv.Value);

            for (int i = 0; i < addresses.Count; i++)
            {
                char area;
                int word, bit;
                ParseBitAddress(addresses[i], out area, out word, out bit);
                ushort w;
                if (valuesByArea.ContainsKey(area) && valuesByArea[area].TryGetValue(word, out w))
                    result[addresses[i]] = (w & (1 << bit)) != 0;
            }
            return result;
        }

        public ushort ReadWord(string address)
        {
            if (_stream == null) throw new InvalidOperationException("PLC에 연결되지 않았습니다.");
            string a = address.Trim().ToUpperInvariant();
            if (a.Length < 2 || a[0] != 'D')
                throw new ArgumentException("현재 WORD 단일 읽기는 D영역만 지원합니다: " + address);

            int word;
            if (!Int32.TryParse(a.Substring(1), out word))
                throw new ArgumentException("D 주소가 잘못되었습니다: " + address);

            Dictionary<int, ushort> vals = ReadAreaWords('D', new List<int>(new int[] { word }));
            ushort value;
            if (!vals.TryGetValue(word, out value))
                throw new IOException(address + " 읽기 응답에 데이터가 없습니다.");
            return value;
        }

        private void WriteAreaWord(char area, int word, ushort value)
        {
            if (_stream == null) throw new InvalidOperationException("PLC에 연결되지 않았습니다.");
            area = Char.ToUpperInvariant(area);
            if (area != 'M' && area != 'D' && area != 'P')
                throw new ArgumentException("지원하지 않는 WORD 쓰기 영역: " + area);

            string xgt = "%" + area + "W" + word.ToString();
            byte[] name = Encoding.ASCII.GetBytes(xgt);

            MemoryStream body = new MemoryStream();
            body.WriteByte(0x58); body.WriteByte(0x00);
            body.WriteByte(0x02); body.WriteByte(0x00);
            body.WriteByte(0x00); body.WriteByte(0x00);
            body.WriteByte(0x01); body.WriteByte(0x00);
            byte[] nameLen = UInt16LE(name.Length);
            body.Write(nameLen, 0, nameLen.Length);
            body.Write(name, 0, name.Length);
            body.WriteByte(0x02); body.WriteByte(0x00);
            body.WriteByte((byte)(value & 0xFF));
            body.WriteByte((byte)((value >> 8) & 0xFF));

            byte[] rb = Exchange(body.ToArray());
            if (rb.Length < 8) throw new IOException("XGT WORD 쓰기 응답이 짧습니다: " + rb.Length);
            int command = ReadUInt16LE(rb, 0);
            if (command != 0x0059) throw new IOException("XGT WORD 쓰기 응답 명령 오류: 0x" + command.ToString("X4"));
            int error = ReadUInt16LE(rb, 6);
            if (error != 0)
            {
                int detail = rb.Length >= 10 ? ReadUInt16LE(rb, 8) : error;
                throw new XgtProtocolException("PLC XGT WORD 쓰기 오류 0x" + detail.ToString("X4") + " (ErrorStatus=0x" + error.ToString("X4") + ")", detail);
            }
        }

        private void WriteMBitByWord(string address, bool value)
        {
            char area;
            int word, bit;
            ParseBitAddress(address, out area, out word, out bit);
            if (area != 'M') throw new ArgumentException("M BIT 전용 함수입니다: " + address);

            ushort mask = (ushort)(1 << bit);
            ushort lastRead = 0;

            // XGB에서 M 비트 ON/OFF를 확실하게 처리하기 위해
            // %MX 직접 쓰기 대신 해당 %MW를 읽고 해당 비트만 변경한 뒤 WORD로 쓴다.
            // M01008 -> MW100 bit8, M01009 -> MW100 bit9, M0100F -> MW100 bit15 ...
            for (int attempt = 0; attempt < 3; attempt++)
            {
                Dictionary<int, ushort> beforeMap = ReadAreaWords('M', new List<int>(new int[] { word }));
                ushort before;
                if (!beforeMap.TryGetValue(word, out before))
                    throw new IOException(address + " 쓰기 전 MW" + word + " 읽기 실패");

                ushort changed;
                if (value) changed = (ushort)(before | mask);
                else changed = (ushort)(before & (ushort)~mask);

                WriteAreaWord('M', word, changed);
                Thread.Sleep(20);

                Dictionary<int, ushort> afterMap = ReadAreaWords('M', new List<int>(new int[] { word }));
                ushort after;
                if (!afterMap.TryGetValue(word, out after))
                    throw new IOException(address + " 쓰기 후 MW" + word + " 읽기 실패");

                lastRead = after;
                bool actual = (after & mask) != 0;
                if (actual == value) return;

                Thread.Sleep(20);
            }

            bool lastState = (lastRead & mask) != 0;
            throw new IOException(address + " " + (value ? "ON" : "OFF") + " 쓰기 후에도 실제 비트가 " + (lastState ? "ON" : "OFF") + "입니다. PLC 래더에서 같은 M비트를 다시 쓰고 있는지 확인하십시오.");
        }

        public void WriteBit(string address, bool value)
        {
            if (_stream == null) throw new InvalidOperationException("PLC에 연결되지 않았습니다.");

            char area;
            int word, bit;
            ParseBitAddress(address, out area, out word, out bit);

            // HMI 스위치에서 사용하는 M영역은 WORD Read-Modify-Write 방식으로 처리한다.
            // 기존 %MX 개별 BIT 쓰기에서 ON은 되지만 OFF가 유지되지 않는 현상을 피한다.
            if (area == 'M')
            {
                WriteMBitByWord(address, value);
                return;
            }

            string xgt = ToXgtBitAddress(address);
            byte[] name = Encoding.ASCII.GetBytes(xgt);

            MemoryStream body = new MemoryStream();
            body.WriteByte(0x58); body.WriteByte(0x00);
            body.WriteByte(0x00); body.WriteByte(0x00);
            body.WriteByte(0x00); body.WriteByte(0x00);
            body.WriteByte(0x01); body.WriteByte(0x00);
            byte[] nameLen = UInt16LE(name.Length);
            body.Write(nameLen, 0, nameLen.Length);
            body.Write(name, 0, name.Length);
            body.WriteByte(0x01); body.WriteByte(0x00);
            body.WriteByte(value ? (byte)0x01 : (byte)0x00);

            byte[] rb = Exchange(body.ToArray());
            if (rb.Length < 8) throw new IOException("XGT BIT 쓰기 응답이 짧습니다: " + rb.Length);
            int command = ReadUInt16LE(rb, 0);
            if (command != 0x0059) throw new IOException("XGT BIT 쓰기 응답 명령 오류: 0x" + command.ToString("X4"));
            int error = ReadUInt16LE(rb, 6);
            if (error != 0)
            {
                int detail = rb.Length >= 10 ? ReadUInt16LE(rb, 8) : error;
                throw new XgtProtocolException("PLC XGT BIT 쓰기 오류 0x" + detail.ToString("X4") + " (ErrorStatus=0x" + error.ToString("X4") + ")", detail);
            }
        }

        public void WriteWord(string address, ushort value)
        {
            if (_stream == null) throw new InvalidOperationException("PLC에 연결되지 않았습니다.");
            string a = address.Trim().ToUpperInvariant();
            if (a.Length < 2 || a[0] != 'D')
                throw new ArgumentException("현재 WORD 단일 쓰기는 D영역만 지원합니다: " + address);

            int word;
            if (!Int32.TryParse(a.Substring(1), out word))
                throw new ArgumentException("D 주소가 잘못되었습니다: " + address);

            WriteAreaWord('D', word, value);
        }

        private void DisposeSocket()
        {
            try { if (_stream != null) _stream.Close(); } catch { }
            try { if (_tcp != null) _tcp.Close(); } catch { }
            _stream = null;
            _tcp = null;
        }

        public void Dispose()
        {
            DisposeSocket();
        }
    }


    public class MainForm : Form
    {
        private TextBox txtIp;
        private NumericUpDown numPort;
        private NumericUpDown numCycle;
        private Button btnConnect;
        private CheckBox chkWriteEnable;
        private Label lblStatus;
        private Label lblProject;
        private TabControl tabs;
        private TabPage tabRun;
        private TabPage tabEdit;
        private Panel canvas;
        private Button btnLayoutMode;
        private DataGridView gridEditor;
        private TextBox txtLog;

        private volatile bool _running;
        private Thread _worker;
        private XgtClient _client;
        private readonly object _sync = new object();
        private readonly object _projectSync = new object();
        private readonly object _cacheSync = new object();
        private int _cycleMs = 300;
        private bool _writeCheckInternal;
        private HmiProject _project;
        private string _projectPath;
        private bool _layoutMode;
        private List<HmiItem> _copyBuffer = new List<HmiItem>();
        private int _pasteSerial = 0;
        private Point _canvasPastePoint = new Point(40, 40);

        private Dictionary<string, bool> _bitCache = new Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase);
        private Dictionary<string, ushort> _wordCache = new Dictionary<string, ushort>(StringComparer.OrdinalIgnoreCase);
        private Dictionary<string, Panel> _runtimeCards = new Dictionary<string, Panel>();
        private Dictionary<string, Label> _runtimeState = new Dictionary<string, Label>();
        private Dictionary<string, Label> _runtimeMonitor = new Dictionary<string, Label>();
        private Dictionary<string, NumericUpDown> _runtimeNumeric = new Dictionary<string, NumericUpDown>();

        private Panel _dragCard;
        private Point _dragStartScreen;
        private Point _dragStartLocation;

        private enum ResizeEdge { None, N, S, E, W, NE, NW, SE, SW }
        private class ResizeTag
        {
            public Panel Card;
            public ResizeEdge Edge;
            public ResizeTag(Panel card, ResizeEdge edge) { Card = card; Edge = edge; }
        }
        private Panel _selectedCard;
        private readonly List<Panel> _resizeHandles = new List<Panel>();
        private ResizeEdge _resizeEdge = ResizeEdge.None;
        private Point _resizeStartScreen;
        private Rectangle _resizeStartBounds;
        private Label lblLayoutInfo;

        public MainForm()
        {
            Text = "XGB XGT HMI Designer v3 - r004";
            Width = 1280;
            Height = 860;
            MinimumSize = new Size(1040, 700);
            StartPosition = FormStartPosition.CenterScreen;
            Font = new Font("Malgun Gothic", 9F);
            FormClosing += OnFormClosing;

            _projectPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "r004_hmi_project.xml");
            _project = LoadProjectOrDefault(_projectPath);
            BuildUi();
            ApplyProjectToConnectionFields();
            FillEditorGrid();
            RenderCanvas();
        }

        private void BuildUi()
        {
            Panel top = new Panel();
            top.Dock = DockStyle.Top;
            top.Height = 104;
            top.Padding = new Padding(10);
            Controls.Add(top);

            Label lIp = new Label(); lIp.Text = "PLC IP"; lIp.AutoSize = true; lIp.Location = new Point(12, 14); top.Controls.Add(lIp);
            txtIp = new TextBox(); txtIp.Width = 128; txtIp.Location = new Point(68, 10); top.Controls.Add(txtIp);

            Label lPort = new Label(); lPort.Text = "Port"; lPort.AutoSize = true; lPort.Location = new Point(210, 14); top.Controls.Add(lPort);
            numPort = new NumericUpDown(); numPort.Minimum = 1; numPort.Maximum = 65535; numPort.Width = 74; numPort.Location = new Point(246, 10); top.Controls.Add(numPort);

            Label lCycle = new Label(); lCycle.Text = "주기(ms)"; lCycle.AutoSize = true; lCycle.Location = new Point(338, 14); top.Controls.Add(lCycle);
            numCycle = new NumericUpDown(); numCycle.Minimum = 100; numCycle.Maximum = 5000; numCycle.Increment = 50; numCycle.Width = 78; numCycle.Location = new Point(397, 10); numCycle.ValueChanged += delegate { _cycleMs = (int)numCycle.Value; }; top.Controls.Add(numCycle);

            btnConnect = new Button(); btnConnect.Text = "연결"; btnConnect.Width = 90; btnConnect.Height = 28; btnConnect.Location = new Point(492, 8); btnConnect.Click += ConnectClick; top.Controls.Add(btnConnect);

            chkWriteEnable = new CheckBox(); chkWriteEnable.Text = "PLC 쓰기 허용"; chkWriteEnable.AutoSize = true; chkWriteEnable.Location = new Point(602, 13); chkWriteEnable.CheckedChanged += WriteEnableChanged; top.Controls.Add(chkWriteEnable);

            lblStatus = new Label(); lblStatus.Text = "● 미연결"; lblStatus.AutoSize = false; lblStatus.Width = 1150; lblStatus.Height = 22; lblStatus.Location = new Point(13, 45); lblStatus.ForeColor = Color.Firebrick; lblStatus.Font = new Font("Malgun Gothic", 9F, FontStyle.Bold); top.Controls.Add(lblStatus);

            lblProject = new Label(); lblProject.AutoSize = false; lblProject.Width = 1150; lblProject.Height = 20; lblProject.Location = new Point(13, 72); lblProject.ForeColor = Color.DimGray; top.Controls.Add(lblProject);

            tabs = new TabControl(); tabs.Dock = DockStyle.Fill; Controls.Add(tabs); tabs.BringToFront();
            tabRun = new TabPage("운전 화면");
            tabEdit = new TabPage("화면 편집");
            TabPage tabLog = new TabPage("통신 로그");
            tabs.TabPages.Add(tabRun); tabs.TabPages.Add(tabEdit); tabs.TabPages.Add(tabLog);

            BuildRunTab();
            BuildEditTab();

            txtLog = new TextBox(); txtLog.Dock = DockStyle.Fill; txtLog.Multiline = true; txtLog.ReadOnly = true; txtLog.ScrollBars = ScrollBars.Both; txtLog.Font = new Font("Consolas", 9F); tabLog.Controls.Add(txtLog);
        }

        private void BuildRunTab()
        {
            FlowLayoutPanel toolbar = new FlowLayoutPanel();
            toolbar.Dock = DockStyle.Top; toolbar.Height = 42; toolbar.Padding = new Padding(6); toolbar.WrapContents = false;
            tabRun.Controls.Add(toolbar);

            btnLayoutMode = new Button(); btnLayoutMode.Text = "배치 편집 OFF"; btnLayoutMode.AutoSize = true; btnLayoutMode.Click += delegate { ToggleLayoutMode(); }; toolbar.Controls.Add(btnLayoutMode);
            Button bApply = new Button(); bApply.Text = "편집내용 다시 적용"; bApply.AutoSize = true; bApply.Click += delegate { ApplyEditorToProject(true); }; toolbar.Controls.Add(bApply);
            Button bSave = new Button(); bSave.Text = "프로젝트 저장"; bSave.AutoSize = true; bSave.Click += delegate { SaveCurrentProject(); }; toolbar.Controls.Add(bSave);
            Label help = new Label(); help.Text = "  배치 편집 ON: 요소를 드래그 이동 / 선택 후 8개 핸들로 크기조절 / 우클릭 복사·복제·삭제"; help.AutoSize = true; help.Margin = new Padding(12, 7, 0, 0); toolbar.Controls.Add(help);
            lblLayoutInfo = new Label(); lblLayoutInfo.Text = ""; lblLayoutInfo.AutoSize = true; lblLayoutInfo.Margin = new Padding(12, 7, 0, 0); lblLayoutInfo.ForeColor = Color.DimGray; toolbar.Controls.Add(lblLayoutInfo);

            canvas = new Panel();
            canvas.Dock = DockStyle.Fill; canvas.AutoScroll = true; canvas.BackColor = Color.White;
            canvas.BorderStyle = BorderStyle.FixedSingle;
            canvas.MouseDown += CanvasMouseDown;
            ContextMenuStrip canvasMenu = new ContextMenuStrip();
            canvasMenu.Items.Add("여기에 붙여넣기", null, PasteToCanvas);
            canvas.ContextMenuStrip = canvasMenu;
            tabRun.Controls.Add(canvas); canvas.BringToFront();
        }

        private void BuildEditTab()
        {
            FlowLayoutPanel tools = new FlowLayoutPanel();
            tools.Dock = DockStyle.Top; tools.Height = 112; tools.Padding = new Padding(6); tools.WrapContents = true;
            tabEdit.Controls.Add(tools);

            AddToolButton(tools, "+ 스위치", delegate { AddEditorItem("SWITCH"); });
            AddToolButton(tools, "+ 램프", delegate { AddEditorItem("LAMP"); });
            AddToolButton(tools, "+ 숫자입력", delegate { AddEditorItem("NUM_INPUT"); });
            AddToolButton(tools, "+ 숫자표시", delegate { AddEditorItem("NUM_DISPLAY"); });
            AddToolButton(tools, "+ 텍스트", delegate { AddEditorItem("TEXT"); });
            AddToolButton(tools, "복사 Ctrl+C", CopySelectedEditorRows);
            AddToolButton(tools, "붙여넣기 Ctrl+V", PasteEditorRows);
            AddToolButton(tools, "선택 복제 ×N", DuplicateSelectedEditorRows);
            AddToolButton(tools, "스위치 수량 추가", AddMultipleSwitches);
            AddToolButton(tools, "선택 삭제", DeleteSelectedEditorRow);
            AddToolButton(tools, "화면 적용", delegate { ApplyEditorToProject(true); });
            AddToolButton(tools, "저장", delegate { SaveCurrentProject(); });
            AddToolButton(tools, "다른 이름으로 저장", SaveProjectAs);
            AddToolButton(tools, "불러오기", LoadProjectDialog);
            AddToolButton(tools, "r004 기본 예제", RestoreDefaultProject);

            Label note = new Label();
            note.Text = "Ctrl+C / Ctrl+V 복사·붙여넣기   |   여러 행 선택   |   선택 복제 ×N   |   운전화면 배치 편집: 드래그 이동 + 8방향 크기조절";
            note.AutoSize = true; note.ForeColor = Color.DimGray; note.Margin = new Padding(10, 8, 0, 0); tools.Controls.Add(note);

            gridEditor = new DataGridView();
            gridEditor.Dock = DockStyle.Fill;
            gridEditor.AllowUserToAddRows = false; gridEditor.AllowUserToDeleteRows = false; gridEditor.RowHeadersVisible = false;
            gridEditor.SelectionMode = DataGridViewSelectionMode.FullRowSelect; gridEditor.MultiSelect = true;
            gridEditor.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None; gridEditor.BackgroundColor = Color.White;
            gridEditor.DataError += delegate { };
            gridEditor.KeyDown += EditorGridKeyDown;
            ContextMenuStrip editMenu = new ContextMenuStrip();
            editMenu.Items.Add("복사", null, CopySelectedEditorRows);
            editMenu.Items.Add("붙여넣기", null, PasteEditorRows);
            editMenu.Items.Add("선택 복제 ×N", null, DuplicateSelectedEditorRows);
            editMenu.Items.Add(new ToolStripSeparator());
            editMenu.Items.Add("선택 삭제", null, DeleteSelectedEditorRow);
            gridEditor.ContextMenuStrip = editMenu;
            tabEdit.Controls.Add(gridEditor); gridEditor.BringToFront();

            DataGridViewCheckBoxColumn use = new DataGridViewCheckBoxColumn(); use.Name="Enabled"; use.HeaderText="사용"; use.Width=45; gridEditor.Columns.Add(use);
            DataGridViewComboBoxColumn type = new DataGridViewComboBoxColumn(); type.Name="Type"; type.HeaderText="형식"; type.Width=105; type.Items.AddRange(new object[]{"SWITCH","LAMP","NUM_INPUT","NUM_DISPLAY","TEXT"}); gridEditor.Columns.Add(type);
            gridEditor.Columns.Add("NameCol", "표시 이름"); gridEditor.Columns["NameCol"].Width=170;
            gridEditor.Columns.Add("Device", "디바이스"); gridEditor.Columns["Device"].Width=100;
            gridEditor.Columns.Add("Monitor", "상태확인 디바이스"); gridEditor.Columns["Monitor"].Width=120;
            DataGridViewComboBoxColumn action = new DataGridViewComboBoxColumn(); action.Name="Action"; action.HeaderText="스위치 동작"; action.Width=100; action.Items.AddRange(new object[]{"토글","ON","OFF","순간","ON/OFF"}); gridEditor.Columns.Add(action);
            gridEditor.Columns.Add("Min", "최소"); gridEditor.Columns["Min"].Width=70;
            gridEditor.Columns.Add("Max", "최대"); gridEditor.Columns["Max"].Width=70;
            gridEditor.Columns.Add("X", "X"); gridEditor.Columns["X"].Width=55;
            gridEditor.Columns.Add("Y", "Y"); gridEditor.Columns["Y"].Width=55;
            gridEditor.Columns.Add("W", "폭"); gridEditor.Columns["W"].Width=55;
            gridEditor.Columns.Add("H", "높이"); gridEditor.Columns["H"].Width=55;
        }

        private void AddToolButton(FlowLayoutPanel p, string text, EventHandler handler)
        {
            Button b = new Button(); b.Text = text; b.AutoSize = true; b.Height = 28; b.Click += handler; p.Controls.Add(b);
        }

        private void ApplyProjectToConnectionFields()
        {
            txtIp.Text = String.IsNullOrWhiteSpace(_project.PlcIp) ? "192.168.1.120" : _project.PlcIp;
            int port = _project.Port < 1 || _project.Port > 65535 ? 2004 : _project.Port;
            numPort.Value = port;
            int cyc = _project.CycleMs < 100 ? 300 : Math.Min(5000, _project.CycleMs);
            numCycle.Value = cyc; _cycleMs = cyc;
            UpdateProjectLabel();
        }

        private void UpdateProjectLabel()
        {
            lblProject.Text = "프로젝트: " + _projectPath + "   |   화면 요소 " + (_project == null || _project.Items == null ? 0 : _project.Items.Count) + "개";
        }

        private HmiProject LoadProjectOrDefault(string path)
        {
            try
            {
                if (File.Exists(path)) return DeserializeProject(path);
            }
            catch { }
            HmiProject p = CreateDefaultProject();
            try { SerializeProject(path, p); } catch { }
            return p;
        }

        private HmiProject DeserializeProject(string path)
        {
            XmlSerializer xs = new XmlSerializer(typeof(HmiProject));
            using (FileStream fs = File.OpenRead(path))
            {
                HmiProject p = (HmiProject)xs.Deserialize(fs);
                if (p.Items == null) p.Items = new List<HmiItem>();
                return p;
            }
        }

        private void SerializeProject(string path, HmiProject p)
        {
            XmlSerializer xs = new XmlSerializer(typeof(HmiProject));
            using (FileStream fs = File.Create(path)) xs.Serialize(fs, p);
        }

        private HmiProject CreateDefaultProject()
        {
            HmiProject p = new HmiProject();
            p.PlcIp = "192.168.1.120"; p.Port = 2004; p.CycleMs = 300;
            string[,] sw = new string[,] {
                {"M01008","P00120","sys tr in enable c"}, {"M01009","P00121","job start"},
                {"M01010","P00122","job exit"}, {"M01011","P00123","job pause"},
                {"M01012","P00124","job restart"}, {"M01013","P00125","alarm reset"},
                {"M01014","P00126","servo on/off"}, {"M01015","P00127","P00127 제어"},
                {"M01016","P00128","P00128 제어"}, {"M01006","P00130","P00130 제어"},
                {"M01007","P00131","P00131 제어"}, {"M00510","P00138","P00138 제어"},
                {"M00501","P00139","P00139 제어"}, {"M01002","P0013A","P0013A 제어"},
                {"M01003","P0013B","P0013B 제어"}, {"M01005","P0013C","외부도어 닫힘"},
                {"M01004","P0013D","외부도어 열림"}, {"M01000","P0013E","P0013E 제어"},
                {"M01001","P0013F","P0013F 제어"}
            };
            for (int i=0; i<sw.GetLength(0); i++)
            {
                int col = i % 5; int row = i / 5;
                HmiItem h = new HmiItem(); h.Type="SWITCH"; h.Name=sw[i,2]; h.Device=sw[i,0]; h.MonitorDevice=sw[i,1]; h.Action="토글";
                h.X=18 + col*220; h.Y=18 + row*118; h.Width=205; h.Height=105; p.Items.Add(h);
            }
            HmiItem n = new HmiItem(); n.Type="NUM_INPUT"; n.Name="D200 설정값"; n.Device="D200"; n.Min=-32768; n.Max=65535; n.X=18; n.Y=500; n.Width=250; n.Height=125; p.Items.Add(n);
            HmiItem d = new HmiItem(); d.Type="NUM_DISPLAY"; d.Name="D100 현재값"; d.Device="D100"; d.X=285; d.Y=500; d.Width=220; d.Height=125; p.Items.Add(d);
            HmiItem t = new HmiItem(); t.Type="TEXT"; t.Name="※ PLC 래더: MOV D200 D100 으로 변경 후 D200 설정 사용"; t.Device=""; t.X=525; t.Y=520; t.Width=520; t.Height=70; p.Items.Add(t);
            return p;
        }

        private void FillEditorGrid()
        {
            if (gridEditor == null) return;
            gridEditor.Rows.Clear();
            lock (_projectSync)
            {
                foreach (HmiItem h in _project.Items)
                {
                    int r = gridEditor.Rows.Add(h.Enabled, h.Type, h.Name, h.Device, h.MonitorDevice, h.Action, h.Min, h.Max, h.X, h.Y, h.Width, h.Height);
                    gridEditor.Rows[r].Tag = h.Id;
                }
            }
        }

        private int IntCell(DataGridViewRow row, string col, int fallback)
        {
            int v; if (Int32.TryParse(Convert.ToString(row.Cells[col].Value), out v)) return v; return fallback;
        }

        private void ApplyEditorToProject(bool showMessage)
        {
            try
            {
                gridEditor.EndEdit();
                List<HmiItem> list = new List<HmiItem>();
                foreach (DataGridViewRow row in gridEditor.Rows)
                {
                    HmiItem h = new HmiItem();
                    string id = Convert.ToString(row.Tag); if (!String.IsNullOrWhiteSpace(id)) h.Id=id;
                    object ev = row.Cells["Enabled"].Value; h.Enabled = ev == null ? true : Convert.ToBoolean(ev);
                    h.Type = Convert.ToString(row.Cells["Type"].Value).Trim().ToUpperInvariant();
                    h.Name = Convert.ToString(row.Cells["NameCol"].Value);
                    h.Device = Convert.ToString(row.Cells["Device"].Value).Trim().ToUpperInvariant();
                    h.MonitorDevice = Convert.ToString(row.Cells["Monitor"].Value).Trim().ToUpperInvariant();
                    h.Action = Convert.ToString(row.Cells["Action"].Value); if (String.IsNullOrWhiteSpace(h.Action)) h.Action="토글";
                    h.Min=IntCell(row,"Min",0); h.Max=IntCell(row,"Max",65535); if (h.Max < h.Min) { int q=h.Min; h.Min=h.Max; h.Max=q; }
                    h.X=Math.Max(0,IntCell(row,"X",20)); h.Y=Math.Max(0,IntCell(row,"Y",20));
                    h.Width=Math.Max(80,IntCell(row,"W",180)); h.Height=Math.Max(55,IntCell(row,"H",100));
                    ValidateItem(h);
                    list.Add(h);
                }
                lock (_projectSync) { _project.Items = list; }
                RenderCanvas(); UpdateProjectLabel();
                if (showMessage) MessageBox.Show("편집 내용을 운전 화면에 적용했습니다.\r\n영구 저장하려면 '저장'을 누르십시오.", "화면 적용", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show("화면 설정 오류\r\n" + ex.Message, "편집 오류", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ValidateItem(HmiItem h)
        {
            if (String.IsNullOrWhiteSpace(h.Type)) throw new ArgumentException("형식이 비어 있습니다.");
            if (h.Type=="TEXT") return;
            if (String.IsNullOrWhiteSpace(h.Device)) throw new ArgumentException("'"+h.Name+"'의 디바이스가 비어 있습니다.");
            char c = Char.ToUpperInvariant(h.Device[0]);
            if (h.Type=="SWITCH" || h.Type=="LAMP")
            {
                if (c!='M' && c!='P') throw new ArgumentException("'"+h.Name+"'은 M 또는 P 비트 주소를 사용해야 합니다.");
                if (!String.IsNullOrWhiteSpace(h.MonitorDevice))
                {
                    char m=Char.ToUpperInvariant(h.MonitorDevice[0]); if (m!='M' && m!='P') throw new ArgumentException("상태확인 디바이스는 M/P 비트만 가능합니다: "+h.Name);
                }
            }
            else if (h.Type=="NUM_INPUT" || h.Type=="NUM_DISPLAY")
            {
                if (c!='D') throw new ArgumentException("'"+h.Name+"'은 D WORD 주소를 사용해야 합니다.");
                if (h.Min < -32768 || h.Max > 65535) throw new ArgumentException("'"+h.Name+"'의 WORD 범위는 -32768 ~ 65535 안에서 설정하십시오.");
            }
            else throw new ArgumentException("지원하지 않는 형식: "+h.Type);
        }

        private void AddEditorItem(string type)
        {
            HmiItem h = new HmiItem(); h.Type=type; h.X=20; h.Y=20; h.Width=180; h.Height=100;
            if (type=="SWITCH") { h.Name="새 스위치"; h.Device="M1000"; h.Action="토글"; }
            else if (type=="LAMP") { h.Name="새 램프"; h.Device="P00000"; }
            else if (type=="NUM_INPUT") { h.Name="새 숫자입력"; h.Device="D200"; h.Width=230; h.Height=120; }
            else if (type=="NUM_DISPLAY") { h.Name="새 숫자표시"; h.Device="D100"; h.Width=210; h.Height=110; }
            else { h.Name="새 텍스트"; h.Device=""; h.Width=300; h.Height=70; }
            int r=gridEditor.Rows.Add(h.Enabled,h.Type,h.Name,h.Device,h.MonitorDevice,h.Action,h.Min,h.Max,h.X,h.Y,h.Width,h.Height); gridEditor.Rows[r].Tag=h.Id; gridEditor.CurrentCell=gridEditor.Rows[r].Cells[2];
        }

        private HmiItem CloneItem(HmiItem src, bool newId)
        {
            HmiItem c = new HmiItem();
            if (!newId) c.Id = src.Id;
            c.Enabled = src.Enabled; c.Type = src.Type; c.Name = src.Name; c.Device = src.Device; c.MonitorDevice = src.MonitorDevice;
            c.Action = src.Action; c.Min = src.Min; c.Max = src.Max; c.X = src.X; c.Y = src.Y; c.Width = src.Width; c.Height = src.Height;
            return c;
        }

        private HmiItem ItemFromEditorRow(DataGridViewRow row)
        {
            HmiItem h = new HmiItem();
            string id = Convert.ToString(row.Tag); if (!String.IsNullOrWhiteSpace(id)) h.Id = id;
            object ev = row.Cells["Enabled"].Value; h.Enabled = ev == null ? true : Convert.ToBoolean(ev);
            h.Type = Convert.ToString(row.Cells["Type"].Value).Trim().ToUpperInvariant();
            h.Name = Convert.ToString(row.Cells["NameCol"].Value);
            h.Device = Convert.ToString(row.Cells["Device"].Value).Trim().ToUpperInvariant();
            h.MonitorDevice = Convert.ToString(row.Cells["Monitor"].Value).Trim().ToUpperInvariant();
            h.Action = Convert.ToString(row.Cells["Action"].Value); if (String.IsNullOrWhiteSpace(h.Action)) h.Action = "토글";
            h.Min = IntCell(row, "Min", 0); h.Max = IntCell(row, "Max", 65535);
            h.X = Math.Max(0, IntCell(row, "X", 20)); h.Y = Math.Max(0, IntCell(row, "Y", 20));
            h.Width = Math.Max(80, IntCell(row, "W", 180)); h.Height = Math.Max(55, IntCell(row, "H", 100));
            return h;
        }

        private int AddItemToEditor(HmiItem h, bool select)
        {
            int r = gridEditor.Rows.Add(h.Enabled, h.Type, h.Name, h.Device, h.MonitorDevice, h.Action, h.Min, h.Max, h.X, h.Y, h.Width, h.Height);
            gridEditor.Rows[r].Tag = h.Id;
            if (select) gridEditor.Rows[r].Selected = true;
            return r;
        }

        private List<DataGridViewRow> SelectedEditorRowsSorted()
        {
            List<DataGridViewRow> rows = new List<DataGridViewRow>();
            foreach (DataGridViewRow r in gridEditor.SelectedRows) rows.Add(r);
            rows.Sort(delegate(DataGridViewRow a, DataGridViewRow b) { return a.Index.CompareTo(b.Index); });
            return rows;
        }

        private void CopySelectedEditorRows(object sender, EventArgs e)
        {
            gridEditor.EndEdit();
            List<DataGridViewRow> rows = SelectedEditorRowsSorted();
            if (rows.Count < 1) { MessageBox.Show("복사할 요소를 선택하십시오.", "복사", MessageBoxButtons.OK, MessageBoxIcon.Information); return; }
            _copyBuffer.Clear();
            foreach (DataGridViewRow row in rows) _copyBuffer.Add(ItemFromEditorRow(row));
            _pasteSerial = 0;
            Log("COPY " + _copyBuffer.Count + " ITEM(S)");
        }

        private void PasteEditorRows(object sender, EventArgs e)
        {
            if (_copyBuffer.Count < 1) { MessageBox.Show("먼저 복사할 요소를 선택하고 Ctrl+C를 누르십시오.", "붙여넣기", MessageBoxButtons.OK, MessageBoxIcon.Information); return; }
            _pasteSerial++;
            int offset = 20 * _pasteSerial;
            gridEditor.ClearSelection();
            int last = -1;
            foreach (HmiItem src in _copyBuffer)
            {
                HmiItem h = CloneItem(src, true); h.X = Math.Max(0, src.X + offset); h.Y = Math.Max(0, src.Y + offset);
                last = AddItemToEditor(h, true);
            }
            if (last >= 0) gridEditor.FirstDisplayedScrollingRowIndex = Math.Max(0, last - 3);
            Log("PASTE " + _copyBuffer.Count + " ITEM(S)");
        }

        private int PromptCount(string title, string message, int defaultValue, int maxValue)
        {
            using (Form f = new Form())
            {
                f.Text = title; f.StartPosition = FormStartPosition.CenterParent; f.FormBorderStyle = FormBorderStyle.FixedDialog;
                f.MinimizeBox = false; f.MaximizeBox = false; f.ClientSize = new Size(350, 125); f.Font = Font;
                Label l = new Label(); l.Text = message; l.AutoSize = true; l.Location = new Point(14, 17); f.Controls.Add(l);
                NumericUpDown n = new NumericUpDown(); n.Minimum = 1; n.Maximum = maxValue; n.Value = Math.Max(1, Math.Min(maxValue, defaultValue)); n.Location = new Point(18, 46); n.Width = 115; f.Controls.Add(n);
                Button ok = new Button(); ok.Text = "확인"; ok.DialogResult = DialogResult.OK; ok.Location = new Point(176, 82); ok.Width = 75; f.Controls.Add(ok);
                Button cancel = new Button(); cancel.Text = "취소"; cancel.DialogResult = DialogResult.Cancel; cancel.Location = new Point(258, 82); cancel.Width = 75; f.Controls.Add(cancel);
                f.AcceptButton = ok; f.CancelButton = cancel;
                return f.ShowDialog(this) == DialogResult.OK ? Decimal.ToInt32(n.Value) : 0;
            }
        }

        private void DuplicateSelectedEditorRows(object sender, EventArgs e)
        {
            gridEditor.EndEdit();
            List<DataGridViewRow> rows = SelectedEditorRowsSorted();
            if (rows.Count < 1) { MessageBox.Show("복제할 요소를 선택하십시오.", "수량 복제", MessageBoxButtons.OK, MessageBoxIcon.Information); return; }
            int count = PromptCount("선택 복제 ×N", "선택한 요소 묶음을 몇 벌 더 만들까요?", 1, 100);
            if (count <= 0) return;
            List<HmiItem> originals = new List<HmiItem>(); foreach (DataGridViewRow row in rows) originals.Add(ItemFromEditorRow(row));
            gridEditor.ClearSelection();
            for (int k = 1; k <= count; k++)
            {
                int offset = 20 * k;
                foreach (HmiItem src in originals)
                {
                    HmiItem h = CloneItem(src, true); h.X = Math.Max(0, src.X + offset); h.Y = Math.Max(0, src.Y + offset); AddItemToEditor(h, true);
                }
            }
            Log("DUPLICATE " + rows.Count + " ITEM(S) x " + count);
        }

        private int FindNextMAddress()
        {
            int max = 999;
            foreach (DataGridViewRow row in gridEditor.Rows)
            {
                string d = Convert.ToString(row.Cells["Device"].Value).Trim().ToUpperInvariant();
                if (d.StartsWith("M")) { int v; if (Int32.TryParse(d.Substring(1), out v) && v > max) max = v; }
            }
            return max + 1;
        }

        private Point FindNextFreeEditorPosition(int index)
        {
            int col = index % 5; int row = index / 5; return new Point(20 + col * 220, 20 + row * 118);
        }

        private void AddMultipleSwitches(object sender, EventArgs e)
        {
            int count = PromptCount("스위치 수량 추가", "새 스위치를 몇 개 추가할까요? (M주소는 자동 증가)", 5, 100);
            if (count <= 0) return;
            int startM = FindNextMAddress(); int baseIndex = gridEditor.Rows.Count; gridEditor.ClearSelection();
            for (int i = 0; i < count; i++)
            {
                HmiItem h = new HmiItem(); h.Type = "SWITCH"; h.Name = "새 스위치 " + (i + 1); h.Device = "M" + (startM + i).ToString(); h.Action = "토글";
                Point pt = FindNextFreeEditorPosition(baseIndex + i); h.X = pt.X; h.Y = pt.Y; h.Width = 180; h.Height = 100; AddItemToEditor(h, true);
            }
            Log("ADD MULTI SWITCH " + count + " / START M" + startM);
        }

        private void EditorGridKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control && e.KeyCode == Keys.C) { CopySelectedEditorRows(sender, EventArgs.Empty); e.Handled = true; e.SuppressKeyPress = true; }
            else if (e.Control && e.KeyCode == Keys.V) { PasteEditorRows(sender, EventArgs.Empty); e.Handled = true; e.SuppressKeyPress = true; }
            else if (e.KeyCode == Keys.Delete) { DeleteSelectedEditorRow(sender, EventArgs.Empty); e.Handled = true; e.SuppressKeyPress = true; }
        }

        private void CanvasMouseDown(object sender, MouseEventArgs e)
        {
            if (_layoutMode && e.Button == MouseButtons.Left) SelectRuntimeCard(null);
            if (e.Button == MouseButtons.Right)
            {
                Point ap = canvas.AutoScrollPosition;
                _canvasPastePoint = new Point(Math.Max(0, e.X - ap.X), Math.Max(0, e.Y - ap.Y));
            }
        }

        private void CopyRuntimeItem(string id)
        {
            HmiItem h = FindItem(id); if (h == null) return;
            _copyBuffer.Clear(); _copyBuffer.Add(CloneItem(h, false)); _pasteSerial = 0; Log("COPY RUNTIME " + h.Name);
        }

        private void DuplicateRuntimeItem(string id)
        {
            HmiItem src = FindItem(id); if (src == null) return;
            HmiItem h = CloneItem(src, true); h.X += 20; h.Y += 20;
            lock (_projectSync) { _project.Items.Add(h); }
            FillEditorGrid(); RenderCanvas(); UpdateProjectLabel(); Log("DUPLICATE RUNTIME " + src.Name);
        }

        private void DeleteRuntimeItem(string id)
        {
            HmiItem h = FindItem(id); if (h == null) return;
            if (MessageBox.Show("'" + h.Name + "' 요소를 삭제할까요?", "삭제", MessageBoxButtons.YesNo, MessageBoxIcon.Question) != DialogResult.Yes) return;
            lock (_projectSync) { _project.Items.RemoveAll(delegate(HmiItem x) { return x.Id == id; }); }
            FillEditorGrid(); RenderCanvas(); UpdateProjectLabel();
        }

        private void PasteToCanvas(object sender, EventArgs e)
        {
            if (_copyBuffer.Count < 1) { MessageBox.Show("먼저 화면 요소를 복사하십시오.", "붙여넣기", MessageBoxButtons.OK, MessageBoxIcon.Information); return; }
            int minX = Int32.MaxValue, minY = Int32.MaxValue;
            foreach (HmiItem x in _copyBuffer) { minX = Math.Min(minX, x.X); minY = Math.Min(minY, x.Y); }
            List<HmiItem> adds = new List<HmiItem>();
            foreach (HmiItem src in _copyBuffer)
            {
                HmiItem h = CloneItem(src, true); h.X = Math.Max(0, _canvasPastePoint.X + (src.X - minX)); h.Y = Math.Max(0, _canvasPastePoint.Y + (src.Y - minY)); adds.Add(h);
            }
            lock (_projectSync) { foreach (HmiItem h in adds) _project.Items.Add(h); }
            FillEditorGrid(); RenderCanvas(); UpdateProjectLabel(); Log("PASTE TO CANVAS " + adds.Count + " ITEM(S)");
        }

        private void DeleteSelectedEditorRow(object sender, EventArgs e)
        {
            List<DataGridViewRow> rows = SelectedEditorRowsSorted();
            if (rows.Count < 1) return;
            if (MessageBox.Show("선택한 화면 요소 " + rows.Count + "개를 삭제할까요?", "삭제", MessageBoxButtons.YesNo, MessageBoxIcon.Question) != DialogResult.Yes) return;
            rows.Sort(delegate(DataGridViewRow a, DataGridViewRow b) { return b.Index.CompareTo(a.Index); });
            foreach (DataGridViewRow row in rows) if (!row.IsNewRow) gridEditor.Rows.RemoveAt(row.Index);
        }

        private void SaveCurrentProject()
        {
            ApplyEditorToProject(false);
            _project.PlcIp=txtIp.Text.Trim(); _project.Port=(int)numPort.Value; _project.CycleMs=(int)numCycle.Value;
            try { SerializeProject(_projectPath,_project); UpdateProjectLabel(); Log("PROJECT SAVE "+_projectPath); MessageBox.Show("저장했습니다.\r\n"+_projectPath,"프로젝트 저장",MessageBoxButtons.OK,MessageBoxIcon.Information); }
            catch(Exception ex){ MessageBox.Show("저장 실패\r\n"+ex.Message,"저장 오류",MessageBoxButtons.OK,MessageBoxIcon.Error); }
        }

        private void SaveProjectAs(object sender, EventArgs e)
        {
            ApplyEditorToProject(false);
            SaveFileDialog s=new SaveFileDialog(); s.Filter="HMI 프로젝트 XML (*.xml)|*.xml"; s.FileName=Path.GetFileName(_projectPath);
            if(s.ShowDialog()!=DialogResult.OK) return;
            _project.PlcIp=txtIp.Text.Trim(); _project.Port=(int)numPort.Value; _project.CycleMs=(int)numCycle.Value;
            try { SerializeProject(s.FileName,_project); _projectPath=s.FileName; UpdateProjectLabel(); Log("PROJECT SAVE AS "+_projectPath); }
            catch(Exception ex){ MessageBox.Show("저장 실패\r\n"+ex.Message,"저장 오류",MessageBoxButtons.OK,MessageBoxIcon.Error); }
        }

        private void LoadProjectDialog(object sender, EventArgs e)
        {
            OpenFileDialog o=new OpenFileDialog(); o.Filter="HMI 프로젝트 XML (*.xml)|*.xml";
            if(o.ShowDialog()!=DialogResult.OK) return;
            try { HmiProject p=DeserializeProject(o.FileName); lock(_projectSync){_project=p;} _projectPath=o.FileName; ApplyProjectToConnectionFields(); FillEditorGrid(); RenderCanvas(); Log("PROJECT LOAD "+_projectPath); }
            catch(Exception ex){ MessageBox.Show("불러오기 실패\r\n"+ex.Message,"프로젝트 오류",MessageBoxButtons.OK,MessageBoxIcon.Error); }
        }

        private void RestoreDefaultProject(object sender, EventArgs e)
        {
            if(MessageBox.Show("현재 화면 편집 내용을 버리고 r004 기본 예제로 되돌릴까요?","기본 예제",MessageBoxButtons.YesNo,MessageBoxIcon.Warning)!=DialogResult.Yes) return;
            lock(_projectSync){_project=CreateDefaultProject();} ApplyProjectToConnectionFields(); FillEditorGrid(); RenderCanvas();
        }

        private HmiItem FindItem(string id)
        {
            lock(_projectSync) { foreach(HmiItem h in _project.Items) if(h.Id==id) return h; }
            return null;
        }

        private List<HmiItem> SnapshotItems()
        {
            List<HmiItem> r=new List<HmiItem>();
            lock(_projectSync)
            {
                foreach(HmiItem h in _project.Items)
                {
                    HmiItem c=new HmiItem(); c.Id=h.Id; c.Enabled=h.Enabled; c.Type=h.Type; c.Name=h.Name; c.Device=h.Device; c.MonitorDevice=h.MonitorDevice; c.Action=h.Action; c.Min=h.Min; c.Max=h.Max; c.X=h.X; c.Y=h.Y; c.Width=h.Width; c.Height=h.Height; r.Add(c);
                }
            }
            return r;
        }

        private void RenderCanvas()
        {
            if (canvas==null) return;
            ClearResizeHandles(); _selectedCard=null; _dragCard=null; _resizeEdge=ResizeEdge.None;
            canvas.SuspendLayout(); canvas.Controls.Clear(); _runtimeCards.Clear(); _runtimeState.Clear(); _runtimeMonitor.Clear(); _runtimeNumeric.Clear();
            int maxX=1100,maxY=650;
            foreach(HmiItem h in SnapshotItems())
            {
                if(!h.Enabled) continue;
                Panel card=CreateRuntimeCard(h); canvas.Controls.Add(card); _runtimeCards[h.Id]=card;
                maxX=Math.Max(maxX,h.X+h.Width+40); maxY=Math.Max(maxY,h.Y+h.Height+40);
            }
            canvas.AutoScrollMinSize=new Size(maxX,maxY); canvas.ResumeLayout();
            UpdateRuntimeFromCaches();
        }

        private Panel CreateRuntimeCard(HmiItem h)
        {
            Panel card=new Panel(); card.Location=new Point(h.X,h.Y); card.Size=new Size(h.Width,h.Height); card.BorderStyle=BorderStyle.FixedSingle; card.BackColor=Color.WhiteSmoke; card.Tag=h.Id;
            Label title=new Label(); title.Name="Title"; title.Text=h.Name; title.Dock=DockStyle.Top; title.Height=26; title.Padding=new Padding(5,4,2,2); title.Font=new Font("Malgun Gothic",9F,FontStyle.Bold); title.BackColor=Color.Gainsboro; card.Controls.Add(title);
            ContextMenuStrip cardMenu = new ContextMenuStrip();
            string menuId = h.Id;
            cardMenu.Items.Add("복사", null, delegate { CopyRuntimeItem(menuId); });
            cardMenu.Items.Add("복제", null, delegate { DuplicateRuntimeItem(menuId); });
            cardMenu.Items.Add(new ToolStripSeparator());
            cardMenu.Items.Add("삭제", null, delegate { DeleteRuntimeItem(menuId); });
            card.ContextMenuStrip = cardMenu; title.ContextMenuStrip = cardMenu;

            if(h.Type=="TEXT")
            {
                title.Dock=DockStyle.Fill; title.TextAlign=ContentAlignment.MiddleLeft; title.Font=new Font("Malgun Gothic",10F,FontStyle.Bold);
                AttachLayoutInteraction(card, card);
                return card;
            }

            Label state=new Label(); state.Name="State"; state.Text="---"; state.AutoSize=false; state.TextAlign=ContentAlignment.MiddleCenter; state.Location=new Point(6,30); state.Size=new Size(Math.Max(60,h.Width-14),24); state.Font=new Font("Malgun Gothic",9F,FontStyle.Bold); card.Controls.Add(state); _runtimeState[h.Id]=state;

            if(h.Type=="SWITCH")
            {
                if(!String.IsNullOrWhiteSpace(h.MonitorDevice))
                {
                    Label mon=new Label(); mon.Name="Monitor"; mon.Text=h.MonitorDevice+": ---"; mon.AutoSize=false; mon.TextAlign=ContentAlignment.MiddleCenter; mon.Location=new Point(6,52); mon.Size=new Size(Math.Max(60,h.Width-14),20); mon.ForeColor=Color.DimGray; card.Controls.Add(mon); _runtimeMonitor[h.Id]=mon;
                }
                int y = String.IsNullOrWhiteSpace(h.MonitorDevice) ? 58 : 73;
                if(h.Action=="ON/OFF")
                {
                    Button on=new Button(); on.Name="OnButton"; on.Text="ON"; on.Location=new Point(6,y); on.Size=new Size(Math.Max(45,(h.Width-20)/2),Math.Max(25,h.Height-y-7)); on.Tag=h.Id; on.Click+=SwitchOnClick; card.Controls.Add(on);
                    Button off=new Button(); off.Name="OffButton"; off.Text="OFF"; off.Location=new Point(on.Right+6,y); off.Size=new Size(Math.Max(45,h.Width-on.Right-13),on.Height); off.Tag=h.Id; off.Click+=SwitchOffClick; card.Controls.Add(off);
                }
                else
                {
                    Button b=new Button(); b.Name="ActionButton"; b.Text=h.Action; b.Location=new Point(6,y); b.Size=new Size(Math.Max(60,h.Width-14),Math.Max(25,h.Height-y-7)); b.Tag=h.Id;
                    if(h.Action=="순간") { b.MouseDown+=MomentaryDown; b.MouseUp+=MomentaryUp; b.MouseLeave+=MomentaryLeave; }
                    else b.Click+=SwitchClick;
                    card.Controls.Add(b);
                }
            }
            else if(h.Type=="LAMP")
            {
                state.Location=new Point(8,35); state.Size=new Size(Math.Max(60,h.Width-18),Math.Max(30,h.Height-44)); state.Font=new Font("Malgun Gothic",14F,FontStyle.Bold);
            }
            else if(h.Type=="NUM_INPUT")
            {
                NumericUpDown num=new NumericUpDown(); num.Name="Numeric"; num.Minimum=h.Min; num.Maximum=h.Max; num.Location=new Point(8,58); num.Width=Math.Max(70,h.Width-95); num.Font=new Font("Consolas",11F,FontStyle.Bold); num.Tag=h.Id; card.Controls.Add(num); _runtimeNumeric[h.Id]=num;
                Button b=new Button(); b.Name="WriteButton"; b.Text="쓰기"; b.Location=new Point(num.Right+5,56); b.Size=new Size(Math.Max(50,h.Width-num.Right-13),30); b.Tag=h.Id; b.Click+=NumericWriteClick; card.Controls.Add(b);
            }
            else if(h.Type=="NUM_DISPLAY")
            {
                state.Location=new Point(8,35); state.Size=new Size(Math.Max(60,h.Width-18),Math.Max(30,h.Height-44)); state.Font=new Font("Consolas",16F,FontStyle.Bold);
            }
            LayoutRuntimeCard(card, h);
            AttachLayoutInteraction(card, card);
            return card;
        }

        private Control FindChildByName(Control parent, string name)
        {
            foreach(Control c in parent.Controls)
            {
                if(c.Name==name) return c;
                Control f=FindChildByName(c,name); if(f!=null) return f;
            }
            return null;
        }

        private void LayoutRuntimeCard(Panel card, HmiItem h)
        {
            if(card==null || h==null) return;
            int w=Math.Max(1,card.ClientSize.Width), ht=Math.Max(1,card.ClientSize.Height);
            Control title=FindChildByName(card,"Title");
            if(h.Type=="TEXT")
            {
                if(title!=null){ title.Dock=DockStyle.None; title.Bounds=new Rectangle(0,0,w,ht); }
                PositionResizeHandles(); return;
            }
            if(title!=null){ title.Dock=DockStyle.None; title.Bounds=new Rectangle(0,0,w,Math.Min(26,ht)); }
            Control state=FindChildByName(card,"State");
            if(h.Type=="SWITCH")
            {
                if(state!=null) state.Bounds=new Rectangle(6,30,Math.Max(10,w-14),24);
                Control mon=FindChildByName(card,"Monitor");
                if(mon!=null) mon.Bounds=new Rectangle(6,52,Math.Max(10,w-14),20);
                int y = mon==null ? 58 : 73;
                int bh=Math.Max(20,ht-y-7);
                Control on=FindChildByName(card,"OnButton"), off=FindChildByName(card,"OffButton"), act=FindChildByName(card,"ActionButton");
                if(on!=null && off!=null)
                {
                    int gap=6, avail=Math.Max(40,w-12-gap), half=avail/2;
                    on.Bounds=new Rectangle(6,y,Math.Max(20,half),bh);
                    off.Bounds=new Rectangle(6+half+gap,y,Math.Max(20,avail-half),bh);
                }
                if(act!=null) act.Bounds=new Rectangle(6,y,Math.Max(20,w-14),bh);
            }
            else if(h.Type=="LAMP" || h.Type=="NUM_DISPLAY")
            {
                if(state!=null) state.Bounds=new Rectangle(8,35,Math.Max(20,w-18),Math.Max(20,ht-44));
            }
            else if(h.Type=="NUM_INPUT")
            {
                Control num=FindChildByName(card,"Numeric"), wb=FindChildByName(card,"WriteButton");
                if(w>=185)
                {
                    int numW=Math.Max(70,w-95);
                    if(num!=null) num.Bounds=new Rectangle(8,58,numW,28);
                    if(wb!=null) wb.Bounds=new Rectangle(13+numW,56,Math.Max(45,w-(13+numW)-8),30);
                }
                else
                {
                    if(num!=null) num.Bounds=new Rectangle(8,54,Math.Max(45,w-16),28);
                    if(wb!=null) wb.Bounds=new Rectangle(8,86,Math.Max(45,w-16),Math.Max(26,ht-94));
                }
            }
            PositionResizeHandles();
        }

        private void AttachLayoutInteraction(Control root, Panel card)
        {
            if(root==null || card==null) return;
            root.MouseDown += DragMouseDown;
            root.MouseMove += DragMouseMove;
            root.MouseUp += DragMouseUp;
            foreach(Control c in root.Controls) AttachLayoutInteraction(c,card);
        }

        private Panel ResolveRuntimeCard(Control c)
        {
            Control cur=c;
            while(cur!=null && cur!=canvas)
            {
                Panel p=cur as Panel;
                if(p!=null && p.Parent==canvas && p.Tag is string) return p;
                cur=cur.Parent;
            }
            return null;
        }

        private void ClearResizeHandles()
        {
            foreach(Panel p in _resizeHandles)
            {
                try { if(p.Parent!=null) p.Parent.Controls.Remove(p); p.Dispose(); } catch { }
            }
            _resizeHandles.Clear();
            if(_selectedCard!=null && !_selectedCard.IsDisposed) _selectedCard.Invalidate();
        }

        private void SelectRuntimeCard(Panel card)
        {
            if(!_layoutMode) return;
            if(_selectedCard==card && card!=null){ PositionResizeHandles(); UpdateLayoutInfo(card); return; }
            ClearResizeHandles();
            _selectedCard=card;
            if(card==null){ if(lblLayoutInfo!=null) lblLayoutInfo.Text=""; return; }
            card.BringToFront();
            AddResizeHandle(card,ResizeEdge.NW,Cursors.SizeNWSE);
            AddResizeHandle(card,ResizeEdge.N,Cursors.SizeNS);
            AddResizeHandle(card,ResizeEdge.NE,Cursors.SizeNESW);
            AddResizeHandle(card,ResizeEdge.E,Cursors.SizeWE);
            AddResizeHandle(card,ResizeEdge.SE,Cursors.SizeNWSE);
            AddResizeHandle(card,ResizeEdge.S,Cursors.SizeNS);
            AddResizeHandle(card,ResizeEdge.SW,Cursors.SizeNESW);
            AddResizeHandle(card,ResizeEdge.W,Cursors.SizeWE);
            PositionResizeHandles();
            UpdateLayoutInfo(card);
            card.Invalidate();
        }

        private void AddResizeHandle(Panel card, ResizeEdge edge, Cursor cursor)
        {
            Panel h=new Panel(); h.Size=new Size(9,9); h.BackColor=SystemColors.Window; h.BorderStyle=BorderStyle.FixedSingle; h.Cursor=cursor; h.Tag=new ResizeTag(card,edge);
            h.MouseDown+=ResizeHandleMouseDown; h.MouseMove+=ResizeHandleMouseMove; h.MouseUp+=ResizeHandleMouseUp;
            card.Controls.Add(h); h.BringToFront(); _resizeHandles.Add(h);
        }

        private void PositionResizeHandles()
        {
            if(_selectedCard==null || _selectedCard.IsDisposed) return;
            int w=_selectedCard.ClientSize.Width, h=_selectedCard.ClientSize.Height, hs=9, hh=hs/2;
            foreach(Panel p in _resizeHandles)
            {
                ResizeTag t=p.Tag as ResizeTag; if(t==null || t.Card!=_selectedCard) continue;
                int x=0,y=0;
                switch(t.Edge)
                {
                    case ResizeEdge.NW: x=0; y=0; break;
                    case ResizeEdge.N: x=Math.Max(0,w/2-hh); y=0; break;
                    case ResizeEdge.NE: x=Math.Max(0,w-hs); y=0; break;
                    case ResizeEdge.E: x=Math.Max(0,w-hs); y=Math.Max(0,h/2-hh); break;
                    case ResizeEdge.SE: x=Math.Max(0,w-hs); y=Math.Max(0,h-hs); break;
                    case ResizeEdge.S: x=Math.Max(0,w/2-hh); y=Math.Max(0,h-hs); break;
                    case ResizeEdge.SW: x=0; y=Math.Max(0,h-hs); break;
                    case ResizeEdge.W: x=0; y=Math.Max(0,h/2-hh); break;
                }
                p.Location=new Point(x,y); p.BringToFront();
            }
        }

        private void ResizeHandleMouseDown(object sender, MouseEventArgs e)
        {
            if(!_layoutMode || e.Button!=MouseButtons.Left) return;
            Panel hp=sender as Panel; ResizeTag t=hp==null?null:hp.Tag as ResizeTag; if(t==null) return;
            SelectRuntimeCard(t.Card); _resizeEdge=t.Edge; _resizeStartScreen=Control.MousePosition; _resizeStartBounds=t.Card.Bounds; _dragCard=null; hp.Capture=true;
        }

        private void ResizeHandleMouseMove(object sender, MouseEventArgs e)
        {
            if(!_layoutMode || _resizeEdge==ResizeEdge.None || _selectedCard==null || e.Button!=MouseButtons.Left) return;
            Point now=Control.MousePosition; int dx=now.X-_resizeStartScreen.X, dy=now.Y-_resizeStartScreen.Y;
            int l=_resizeStartBounds.Left, t=_resizeStartBounds.Top, r=_resizeStartBounds.Right, b=_resizeStartBounds.Bottom;
            if(_resizeEdge==ResizeEdge.W || _resizeEdge==ResizeEdge.NW || _resizeEdge==ResizeEdge.SW) l+=dx;
            if(_resizeEdge==ResizeEdge.E || _resizeEdge==ResizeEdge.NE || _resizeEdge==ResizeEdge.SE) r+=dx;
            if(_resizeEdge==ResizeEdge.N || _resizeEdge==ResizeEdge.NW || _resizeEdge==ResizeEdge.NE) t+=dy;
            if(_resizeEdge==ResizeEdge.S || _resizeEdge==ResizeEdge.SW || _resizeEdge==ResizeEdge.SE) b+=dy;
            int minW=80, minH=55;
            if(r-l<minW){ if(_resizeEdge==ResizeEdge.W || _resizeEdge==ResizeEdge.NW || _resizeEdge==ResizeEdge.SW) l=r-minW; else r=l+minW; }
            if(b-t<minH){ if(_resizeEdge==ResizeEdge.N || _resizeEdge==ResizeEdge.NW || _resizeEdge==ResizeEdge.NE) t=b-minH; else b=t+minH; }
            if(l<0){ if(_resizeEdge==ResizeEdge.W || _resizeEdge==ResizeEdge.NW || _resizeEdge==ResizeEdge.SW) l=0; }
            if(t<0){ if(_resizeEdge==ResizeEdge.N || _resizeEdge==ResizeEdge.NW || _resizeEdge==ResizeEdge.NE) t=0; }
            _selectedCard.Bounds=Rectangle.FromLTRB(l,t,r,b);
            HmiItem item=FindItem(Convert.ToString(_selectedCard.Tag)); if(item!=null) LayoutRuntimeCard(_selectedCard,item);
            UpdateLayoutInfo(_selectedCard);
        }

        private void ResizeHandleMouseUp(object sender, MouseEventArgs e)
        {
            Panel hp=sender as Panel; if(hp!=null) hp.Capture=false;
            if(_resizeEdge==ResizeEdge.None || _selectedCard==null) return;
            CommitCardBounds(_selectedCard); _resizeEdge=ResizeEdge.None; PositionResizeHandles();
        }

        private void CommitCardBounds(Panel card)
        {
            if(card==null) return;
            HmiItem h=FindItem(Convert.ToString(card.Tag));
            if(h!=null){ h.X=card.Left; h.Y=card.Top; h.Width=card.Width; h.Height=card.Height; FillEditorGrid(); }
            int maxX=1100,maxY=650;
            foreach(HmiItem x in SnapshotItems()){ maxX=Math.Max(maxX,x.X+x.Width+40); maxY=Math.Max(maxY,x.Y+x.Height+40); }
            canvas.AutoScrollMinSize=new Size(maxX,maxY); UpdateLayoutInfo(card);
        }

        private void UpdateLayoutInfo(Panel card)
        {
            if(lblLayoutInfo==null) return;
            if(card==null){ lblLayoutInfo.Text=""; return; }
            HmiItem h=FindItem(Convert.ToString(card.Tag)); string n=h==null?"선택":h.Name;
            lblLayoutInfo.Text=n+"  X="+card.Left+" Y="+card.Top+"  W="+card.Width+" H="+card.Height;
        }

        private void ToggleLayoutMode()
        {
            _layoutMode=!_layoutMode;
            btnLayoutMode.Text=_layoutMode ? "배치 편집 ON" : "배치 편집 OFF";
            btnLayoutMode.BackColor=_layoutMode ? Color.Khaki : SystemColors.Control;
            if(!_layoutMode){ ClearResizeHandles(); _selectedCard=null; _dragCard=null; _resizeEdge=ResizeEdge.None; if(lblLayoutInfo!=null) lblLayoutInfo.Text=""; }
            Log("LAYOUT MODE "+(_layoutMode?"ON":"OFF"));
        }

        private void DragMouseDown(object sender, MouseEventArgs e)
        {
            if(!_layoutMode) return;
            Control c=sender as Control; Panel card=ResolveRuntimeCard(c); if(card==null) return;
            if(e.Button==MouseButtons.Right){ SelectRuntimeCard(card); return; }
            if(e.Button!=MouseButtons.Left || _resizeEdge!=ResizeEdge.None) return;
            SelectRuntimeCard(card);
            _dragCard=card; _dragStartScreen=Control.MousePosition; _dragStartLocation=card.Location;
            if(c!=null) c.Capture=true;
        }
        private void DragMouseMove(object sender, MouseEventArgs e)
        {
            if(!_layoutMode || _dragCard==null || _resizeEdge!=ResizeEdge.None || e.Button!=MouseButtons.Left) return;
            Point now=Control.MousePosition; int nx=Math.Max(0,_dragStartLocation.X+now.X-_dragStartScreen.X); int ny=Math.Max(0,_dragStartLocation.Y+now.Y-_dragStartScreen.Y); _dragCard.Location=new Point(nx,ny);
            PositionResizeHandles(); UpdateLayoutInfo(_dragCard);
        }
        private void DragMouseUp(object sender, MouseEventArgs e)
        {
            Control c=sender as Control; if(c!=null) c.Capture=false;
            if(_dragCard==null || _resizeEdge!=ResizeEdge.None) return;
            CommitCardBounds(_dragCard); _dragCard=null;
        }

        private bool EnsureCanWrite()
        {
            if(_layoutMode) { MessageBox.Show("배치 편집 모드에서는 PLC 조작을 하지 않습니다.\r\n배치 편집을 OFF로 바꾸십시오.","배치 편집",MessageBoxButtons.OK,MessageBoxIcon.Information); return false; }
            if(!chkWriteEnable.Checked){ MessageBox.Show("상단의 'PLC 쓰기 허용'을 체크하십시오.","쓰기 잠금",MessageBoxButtons.OK,MessageBoxIcon.Information); return false; }
            if(!_running || _client==null){ MessageBox.Show("PLC에 먼저 연결하십시오.","미연결",MessageBoxButtons.OK,MessageBoxIcon.Information); return false; }
            return true;
        }

        private void SwitchClick(object sender, EventArgs e)
        {
            if(_layoutMode) return;
            Button b=(Button)sender; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h==null || !EnsureCanWrite()) return;

            bool value=false;
            if(h.Action=="ON")
            {
                value=true;
            }
            else if(h.Action=="OFF")
            {
                value=false;
            }
            else
            {
                // 토글은 화면 캐시를 기준으로 하지 않는다.
                // 클릭 순간 PLC에서 실제 Device 상태를 직접 읽은 뒤 반전해서 쓴다.
                // 이렇게 해야 첫 ON 직후 주기 갱신이 늦더라도 두 번째 클릭이 다시 ON을 보내는 문제가 없다.
                bool cur;
                try
                {
                    Dictionary<string,bool> fresh;
                    lock(_sync)
                    {
                        if(_client==null) throw new InvalidOperationException("PLC 연결이 없습니다.");
                        fresh=_client.ReadBits(new List<string>(new string[]{h.Device}));
                    }
                    if(!fresh.TryGetValue(h.Device,out cur))
                        throw new IOException(h.Device+" 현재값 읽기 응답이 없습니다.");

                    lock(_cacheSync){ _bitCache[h.Device]=cur; }
                    value=!cur;
                    Log("TOGGLE READ "+h.Device+" = "+(cur?"ON":"OFF")+" -> WRITE "+(value?"ON":"OFF"));
                }
                catch(Exception ex)
                {
                    Log("TOGGLE READ ERROR "+h.Device+": "+ex.Message);
                    MessageBox.Show(h.Device+" 현재 상태를 읽지 못해 토글하지 않았습니다.\r\n"+ex.Message,"XGT 토글",MessageBoxButtons.OK,MessageBoxIcon.Error);
                    return;
                }
            }

            WriteBitForItem(h,value);
        }
        private void SwitchOnClick(object sender, EventArgs e){ if(_layoutMode) return; Button b=(Button)sender; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h!=null && EnsureCanWrite()) WriteBitForItem(h,true); }
        private void SwitchOffClick(object sender, EventArgs e){ if(_layoutMode) return; Button b=(Button)sender; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h!=null && EnsureCanWrite()) WriteBitForItem(h,false); }
        private void MomentaryDown(object sender, MouseEventArgs e){ if(_layoutMode) return; if(e.Button!=MouseButtons.Left) return; Button b=(Button)sender; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h!=null && EnsureCanWrite()) WriteBitForItem(h,true); }
        private void MomentaryUp(object sender, MouseEventArgs e){ if(_layoutMode) return; Button b=(Button)sender; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h!=null && chkWriteEnable.Checked && _running && !_layoutMode) WriteBitForItem(h,false); }
        private void MomentaryLeave(object sender, EventArgs e){ if(_layoutMode) return; Button b=(Button)sender; if((Control.MouseButtons & MouseButtons.Left)==0) return; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h!=null && chkWriteEnable.Checked && _running && !_layoutMode) WriteBitForItem(h,false); }

        private void WriteBitForItem(HmiItem h, bool value)
        {
            try
            {
                Dictionary<string,bool> verify;
                lock(_sync)
                {
                    if(_client==null) throw new InvalidOperationException("PLC 연결이 없습니다.");
                    _client.WriteBit(h.Device,value);
                    // PLC가 쓴 값을 반영할 시간을 조금 준 뒤 실제 Device를 다시 읽는다.
                    Thread.Sleep(30);
                    verify=_client.ReadBits(new List<string>(new string[]{h.Device}));
                }

                bool rb;
                bool haveReadback=verify.TryGetValue(h.Device,out rb);
                if(haveReadback)
                {
                    lock(_cacheSync){ _bitCache[h.Device]=rb; }
                }
                UpdateRuntimeFromCaches();

                Log("BIT WRITE "+h.Device+" <- "+(value?"ON":"OFF")+(haveReadback?" / READBACK="+(rb?"ON":"OFF"):""));

                if(haveReadback && rb!=value)
                {
                    Log("WARNING "+h.Device+" requested "+(value?"ON":"OFF")+" but PLC readback is "+(rb?"ON":"OFF")+". PLC ladder/other write may be overwriting this bit.");
                }
            }
            catch(Exception ex){ Log("BIT WRITE ERROR "+h.Device+": "+ex.Message); MessageBox.Show(h.Device+" 쓰기 실패\r\n"+ex.Message,"XGT BIT 쓰기",MessageBoxButtons.OK,MessageBoxIcon.Error); }
        }

        private void NumericWriteClick(object sender, EventArgs e)
        {
            if(_layoutMode) return;
            Button b=(Button)sender; HmiItem h=FindItem(Convert.ToString(b.Tag)); if(h==null || !EnsureCanWrite()) return;
            NumericUpDown num; if(!_runtimeNumeric.TryGetValue(h.Id,out num)) return; int entered=Decimal.ToInt32(num.Value); ushort raw=unchecked((ushort)entered);
            try { ushort rb; lock(_sync){ if(_client==null) throw new InvalidOperationException("PLC 연결이 없습니다."); _client.WriteWord(h.Device,raw); rb=_client.ReadWord(h.Device);} lock(_cacheSync){ _wordCache[h.Device]=rb; } UpdateRuntimeFromCaches(); Log("WORD WRITE "+h.Device+" <- "+entered+" / READBACK="+rb); }
            catch(Exception ex){ Log("WORD WRITE ERROR "+h.Device+": "+ex.Message); MessageBox.Show(h.Device+" 쓰기 실패\r\n"+ex.Message,"XGT WORD 쓰기",MessageBoxButtons.OK,MessageBoxIcon.Error); }
        }

        private void UpdateRuntimeFromCaches()
        {
            Dictionary<string,bool> bits; Dictionary<string,ushort> words;
            lock(_cacheSync)
            {
                bits=new Dictionary<string,bool>(_bitCache,StringComparer.OrdinalIgnoreCase);
                words=new Dictionary<string,ushort>(_wordCache,StringComparer.OrdinalIgnoreCase);
            }
            foreach(HmiItem h in SnapshotItems())
            {
                if(!h.Enabled) continue;
                Label s; if(_runtimeState.TryGetValue(h.Id,out s))
                {
                    if(h.Type=="SWITCH" || h.Type=="LAMP")
                    {
                        bool v; if(bits.TryGetValue(h.Device,out v)) SetBitLabel(s,v,h.Device); else { s.Text=h.Device+" : ---"; s.BackColor=Color.Gainsboro; s.ForeColor=Color.DimGray; }
                    }
                    else if(h.Type=="NUM_INPUT" || h.Type=="NUM_DISPLAY")
                    {
                        ushort w; if(words.TryGetValue(h.Device,out w)) { short si=unchecked((short)w); s.Text=h.Device+" = "+w+"  (signed "+si+")"; } else s.Text=h.Device+" = ---";
                    }
                }
                Label m; if(_runtimeMonitor.TryGetValue(h.Id,out m) && !String.IsNullOrWhiteSpace(h.MonitorDevice))
                {
                    bool mv; if(bits.TryGetValue(h.MonitorDevice,out mv)) { m.Text=h.MonitorDevice+" : "+(mv?"● ON":"○ OFF"); m.ForeColor=mv?Color.ForestGreen:Color.DimGray; } else { m.Text=h.MonitorDevice+" : ---"; m.ForeColor=Color.DimGray; }
                }
            }
        }

        private void SetBitLabel(Label l, bool value, string addr)
        {
            l.Text=addr+" : "+(value?"● ON":"○ OFF"); l.BackColor=value?Color.LimeGreen:Color.Gainsboro; l.ForeColor=value?Color.White:Color.DimGray;
        }

        private void WriteEnableChanged(object sender, EventArgs e)
        {
            if(_writeCheckInternal) return;
            if(chkWriteEnable.Checked)
            {
                if(MessageBox.Show("PLC 쓰기를 허용하면 운전 화면의 스위치/숫자입력 명령이 즉시 PLC에 전송됩니다.\r\n\r\n설비 정지와 안전 상태를 확인했습니까?","PLC 쓰기 허용",MessageBoxButtons.YesNo,MessageBoxIcon.Warning)!=DialogResult.Yes)
                { _writeCheckInternal=true; chkWriteEnable.Checked=false; _writeCheckInternal=false; return; }
                Log("PLC WRITE ENABLED");
            }
            else Log("PLC WRITE LOCKED");
        }

        private void ConnectClick(object sender, EventArgs e){ if(_running) Disconnect(); else Connect(); }
        private void Connect()
        {
            try
            {
                SetStatus("● 연결 중...",Color.DarkOrange);
                _client=new XgtClient(txtIp.Text.Trim(),(int)numPort.Value,1800); _client.Connect();
                Dictionary<string,bool> probe; lock(_sync){ probe=_client.ReadBits(new List<string>(new string[]{"P00000"})); }
                lock(_cacheSync){ foreach(KeyValuePair<string,bool> kv in probe) _bitCache[kv.Key]=kv.Value; }
                _running=true; btnConnect.Text="연결 해제"; txtIp.Enabled=false; numPort.Enabled=false; _cycleMs=(int)numCycle.Value;
                _worker=new Thread(WorkerLoop); _worker.IsBackground=true; _worker.Start(); SetStatus("● XGT 통신 정상  "+DateTime.Now.ToString("HH:mm:ss.fff"),Color.ForestGreen);
                Log("XGT Connected: "+txtIp.Text.Trim()+":"+numPort.Value+" / "+_client.ProfileName); Log(_client.NegotiationLog.TrimEnd());
            }
            catch(Exception ex)
            {
                string diag=_client!=null?_client.NegotiationLog:""; if(_client!=null)_client.Dispose(); _client=null; btnConnect.Text="연결"; txtIp.Enabled=true; numPort.Enabled=true; SetStatus("● 연결 실패 - "+ex.Message,Color.Firebrick); Log("CONNECT/XGT ERROR: "+ex.Message); if(!String.IsNullOrEmpty(diag))Log(diag.TrimEnd());
            }
        }
        private void Disconnect()
        {
            _running=false; lock(_sync){ if(_client!=null)_client.Dispose(); _client=null; }
            btnConnect.Text="연결"; txtIp.Enabled=true; numPort.Enabled=true; _writeCheckInternal=true; chkWriteEnable.Checked=false; _writeCheckInternal=false; SetStatus("● 미연결",Color.Firebrick); Log("Disconnected");
        }

        private void AddUnique(List<string> list, string value)
        {
            if(String.IsNullOrWhiteSpace(value)) return; foreach(string s in list) if(s.Equals(value,StringComparison.OrdinalIgnoreCase)) return; list.Add(value);
        }

        private void WorkerLoop()
        {
            while(_running)
            {
                try
                {
                    List<HmiItem> items=SnapshotItems(); List<string> bits=new List<string>(); List<string> words=new List<string>();
                    foreach(HmiItem h in items)
                    {
                        if(!h.Enabled) continue;
                        if(h.Type=="SWITCH" || h.Type=="LAMP") { AddUnique(bits,h.Device); AddUnique(bits,h.MonitorDevice); }
                        else if(h.Type=="NUM_INPUT" || h.Type=="NUM_DISPLAY") AddUnique(words,h.Device);
                    }
                    Dictionary<string,bool> bvals=new Dictionary<string,bool>(StringComparer.OrdinalIgnoreCase);
                    for(int i=0;i<bits.Count;i+=16)
                    {
                        List<string> chunk=new List<string>(); for(int j=i;j<Math.Min(i+16,bits.Count);j++)chunk.Add(bits[j]);
                        Dictionary<string,bool> part; lock(_sync){ if(_client==null)return; part=_client.ReadBits(chunk); } foreach(KeyValuePair<string,bool> kv in part)bvals[kv.Key]=kv.Value;
                    }
                    Dictionary<string,ushort> wvals=new Dictionary<string,ushort>(StringComparer.OrdinalIgnoreCase);
                    foreach(string d in words){ ushort v; lock(_sync){ if(_client==null)return; v=_client.ReadWord(d);} wvals[d]=v; }
                    lock(_cacheSync){ foreach(KeyValuePair<string,bool> kv in bvals)_bitCache[kv.Key]=kv.Value; foreach(KeyValuePair<string,ushort> kv in wvals)_wordCache[kv.Key]=kv.Value; }
                    if(!_running)break;
                    BeginInvoke((MethodInvoker)delegate{ UpdateRuntimeFromCaches(); SetStatus("● 통신 정상  "+DateTime.Now.ToString("HH:mm:ss.fff"),Color.ForestGreen); });
                }
                catch(Exception ex)
                {
                    try{ BeginInvoke((MethodInvoker)delegate{ SetStatus("● 통신 오류 - "+ex.Message,Color.Firebrick); Log("READ ERROR: "+ex.Message); }); }catch{}
                    Thread.Sleep(700);
                }
                Thread.Sleep(_cycleMs);
            }
        }

        private void SetStatus(string text, Color color){ lblStatus.Text=text; lblStatus.ForeColor=color; }
        private void Log(string s){ if(txtLog==null)return; txtLog.AppendText(DateTime.Now.ToString("HH:mm:ss.fff")+"  "+s+Environment.NewLine); if(txtLog.TextLength>40000)txtLog.Text=txtLog.Text.Substring(txtLog.TextLength-25000); }
        private void OnFormClosing(object sender, FormClosingEventArgs e){ _running=false; lock(_sync){ if(_client!=null)_client.Dispose(); _client=null; } }

        [STAThread]
        public static void Main(){ Application.EnableVisualStyles(); Application.SetCompatibleTextRenderingDefault(false); Application.Run(new MainForm()); }
    }
}
