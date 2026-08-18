namespace XgbHmi.App

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Markup.Xaml
open XgbHmi.App.Services
open XgbHmi.App.Views

type App() =
    inherit Application()

    override this.Initialize() = AvaloniaXamlLoader.Load this

    override this.OnFrameworkInitializationCompleted() =
        let settings = AppSettings.load ()
        ThemeService.applyCode settings.Theme

        let langCode =
            if System.String.IsNullOrWhiteSpace settings.Language then XgbHmi.Core.I18n.detectSystemLanguage ()
            else settings.Language
        XgbHmi.Core.I18n.setLanguage langCode

        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- MainWindow.create settings
            desktop.ShutdownMode <- Avalonia.Controls.ShutdownMode.OnMainWindowClose
        | _ -> ()

        base.OnFrameworkInitializationCompleted()
