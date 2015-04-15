package "emacs24-nox"

directory "#{Dir.home("vagrant")}/.emacs.d" do
  owner "vagrant"
  group "vagrant"
end

cookbook_file "#{Dir.home("vagrant")}/.emacs.d/init.el" do
  owner "vagrant"
  group "vagrant"
end
