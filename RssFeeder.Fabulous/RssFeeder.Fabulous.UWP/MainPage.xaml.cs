using Microsoft.FSharp.Core;
using SQLite;
using System;
using Windows.Foundation;
using Windows.UI.ViewManagement;

namespace RssFeeder.Fabulous.UWP
{
    public sealed partial class MainPage
    {
        public MainPage()
        {
            var systemPath = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
            var dbPath = System.IO.Path.Combine(systemPath, "RssFeeder.db3");

            InitializeComponent();
            LoadApplication(new RssFeeder.Fabulous.App(new FSharpOption<string>(dbPath)));
        }
    }
}
