{
  language_server-protocol = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0gvb1j8xsqxms9mww01rmdl78zkd72zgxaap56bhv8j45z05hp1x";
      type = "gem";
    };
    version = "3.17.0.3";
  };
  ruby-lsp = {
    dependencies = ["language_server-protocol" "sorbet-runtime" "yarp"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "17s7wpamkjw83ynqrjsr5ds2yqajbrm0plcylwv19h2j0zi516pk";
      type = "gem";
    };
    version = "0.11.2";
  };
  sorbet-runtime = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "046z5kxd91ibkj1jhsa8vxr7vzgk0a2j13rzd2vn3rpmqk92kfdc";
      type = "gem";
    };
    version = "0.5.11074";
  };
  yarp = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1nqmiw4477afgk2n9ily1gvk78hvg7zj24cdah10m8m8va21r9ir";
      type = "gem";
    };
    version = "0.12.0";
  };
}
