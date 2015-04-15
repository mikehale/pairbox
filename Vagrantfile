Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/trusty64"

  config.vm.synced_folder File.expand_path("~/Projects"), "/projects"

  config.vm.provider "virtualbox" do |vb|
    vb.customize ["modifyvm", :id, "--memory", 2048]
  end

  config.omnibus.chef_version = :latest

  config.vm.provision "chef_solo" do |chef|
    chef.add_recipe "tree"
    chef.add_recipe "apt"
    chef.add_recipe "git"
    chef.add_recipe "mercurial"
    chef.add_recipe "the_silver_searcher"
    chef.add_recipe "ruby"
    chef.add_recipe "go"
    chef.add_recipe "nodejs"
    chef.add_recipe "postgresql"
    chef.add_recipe "redis"
    chef.add_recipe "emacs"
    chef.add_recipe "tmux"
  end

  # Needed to resolve https://github.com/mitchellh/vagrant/issues/5199
  config.trigger.after [:reload, :halt], stdout: true do
    `find #{File.expand_path(File.dirname(__FILE__) + "/.vagrant")} -name synced_folders -exec rm {} \\;`
  end
end
