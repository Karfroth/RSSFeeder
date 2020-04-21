using Windows.Foundation;
using Windows.UI.ViewManagement;

namespace RssFeeder.Fabulous.UWP
{
    public sealed partial class MainPage
    {
        public MainPage()
        {
            InitializeComponent();
            LoadApplication(new RssFeeder.Fabulous.App());
        }
    }
}
