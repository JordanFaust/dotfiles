# frozen_string_literal: true

# vi: set ft=ruby :
# rubocop:disable Metrics/BlockLength
Vagrant.configure('2') do |config|
  config.vm.box = 'nixbox/nixos'
  config.vm.box_version = '23.11'

  # config.vm.provider 'virtualbox' do |vb|
  #   # Display the VirtualBox GUI when booting the machine
  #   vb.gui = true
  #
  #   vb.linked_clone = true
  #   # Customize the amount of memory on the VM:
  #   vb.memory = '8192'
  #   vb.customize ['modifyvm', :id, '--vram', '128']
  #   vb.customize ['modifyvm', :id, '--hwvirtex', 'on']
  #   vb.customize ['modifyvm', :id, '--graphicscontroller', 'vmsvga']
  # end

  config.vm.provider :libvirt do |libvert|
    libvert.memory = '8192'
    libvert.cpus = 4
  end

  # config.vm.provision 'file', source: '/etc/dotfiles/bin', destination: 'dotfiles/bin'
  # config.vm.provision 'file', source: '/etc/dotfiles/config', destination: 'dotfiles/config'
  # config.vm.provision 'file', source: '/etc/dotfiles/default.nix', destination: 'dotfiles/default.nix'
  # config.vm.provision 'file', source: '/etc/dotfiles/firefox', destination: 'dotfiles/firefox'
  # config.vm.provision 'file', source: '/etc/dotfiles/flake.lock', destination: 'dotfiles/flake.lock'
  # config.vm.provision 'file', source: '/etc/dotfiles/flake.nix', destination: 'dotfiles/flake.nix'
  # config.vm.provision 'file', source: '/etc/dotfiles/hosts', destination: 'dotfiles/hosts'
  # config.vm.provision 'file', source: '/etc/dotfiles/lib', destination: 'dotfiles/lib'
  # config.vm.provision 'file', source: '/etc/dotfiles/modules', destination: 'dotfiles/modules'
  # config.vm.provision 'file', source: '/etc/dotfiles/overlays', destination: 'dotfiles/overlays'
  # config.vm.provision 'file', source: '/etc/dotfiles/packages', destination: 'dotfiles/packages'
  # config.vm.provision 'file', source: '/etc/dotfiles/templates', destination: 'dotfiles/templates'
  # config.vm.synced_folders = [
  #   '/etc/dotfiles/bin',
  #   '/etc/dotfiles/config',
  #   '/etc/dotfiles/default.nix',
  #   '/etc/dotfiles/firefox',
  #   '/etc/dotfiles/flake.lock',
  #   '/etc/dotfiles/flake.nix',
  #   '/etc/dotfiles/hosts',
  #   '/etc/dotfiles/lib',
  #   '/etc/dotfiles/modules',
  #   '/etc/dotfiles/overlays',
  #   '/etc/dotfiles/packages',
  #   '/etc/dotfiles/templates'
  # ]
  config.vm.synced_folder '.', '/etc/dotfiles', owner: 'vagrant', group: 'vagrant'

  config.vm.provision 'shell', inline: <<-SHELL
    ls -al /etc/dotfiles
    sudo nix-channel --add https://nixos.org/channels/nixos-23.11 nixos
    sudo nix-env --delete-generations old
    sudo nix-env --install git
  SHELL
  config.vm.provision 'shell', inline: <<-SHELL
    nixos-rebuild --flake /etc/dotfiles#system76 --option pure-eval no   switch
  SHELL
end
# rubocop:enable Metrics/BlockLength
