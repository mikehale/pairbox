Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/trusty64"

  config.vm.network "private_network", type: "dhcp"
  config.vm.synced_folder File.expand_path("~/Projects"), "/projects", type: "nfs", mount_options: ['rw', 'vers=3', 'tcp', 'fsc']

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 2048
    vb.cpus = 2
  end

  config.vm.provision :ansible do |ansible|
    ansible.playbook = "playbook.yml"
  end
end
