namespace RssFeeder.Fabulous.WPF

open System

open Xamarin.Forms
open Xamarin.Forms.Platform.WPF

type MainWindow() = 
    inherit FormsApplicationPage()

module Main = 
    [<EntryPoint>]
    [<STAThread>]
    let main(_args) =

        let systemPath = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
        let dbPath = System.IO.Path.Combine(systemPath, "RssFeeder.db3");

        let app = new System.Windows.Application()
        Forms.Init()
        let window = MainWindow() 
        window.LoadApplication(new RssFeeder.Fabulous.App(Some dbPath))

        app.Run(window)
