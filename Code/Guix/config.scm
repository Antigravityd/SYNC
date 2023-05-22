;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu) (gnu packages audio) (gnu packages shells))
(use-service-modules desktop networking ssh xorg dbus sound pm)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout (keyboard-layout "us" #:options '("ctrl:swapcaps" "compose:menu")))
  (host-name "arete")
  (users (cons* (user-account
                  (name "dnw")
                  (comment "Duncan")
                  (group "users")
		  (shell (file-append zsh "/bin/zsh"))
                  (home-directory "/home/dnw")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
     (list (specification->package "nss-certs")
	   (specification->package "xinit")
	   (specification->package "xorg-server")
	   (specification->package "xf86-input-libinput")
	   (specification->package "xf86-video-intel")
	   (specification->package "emacs")
	   (specification->package "bluez")
	   (specification->package "wpa-supplicant")
	   (specification->package "bluez-alsa")
	   (specification->package "pulseaudio"))
      %base-packages))
  (services
    (append
     (list (service dhcp-client-service-type)
	   (dbus-service #:services (list bluez-alsa))
	   (service polkit-service-type)
	   (service alsa-service-type)
	   (service pulseaudio-service-type)
	   (service elogind-service-type)
	   (service wpa-supplicant-service-type (wpa-supplicant-configuration
						 (interface "wlp2s0")
						 (config-file "/etc/wpa-supplicant/conf")))
	   (service openntpd-service-type)
	   (service usb-modeswitch-service-type)
	   (service upower-service-type)
	   (service tlp-service-type)
	   (service xorg-server-service-type
		    (xorg-configuration
		     (keyboard-layout keyboard-layout)))
	   (bluetooth-service #:auto-enable? #t))
      %base-services))

  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")
      (keyboard-layout keyboard-layout)))

  (swap-devices
    (list (uuid "d2ce3203-c740-4f16-947f-1bc9ff103353")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "cf097cb8-dec8-4d0f-91b6-d2b5949c0d5d"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
