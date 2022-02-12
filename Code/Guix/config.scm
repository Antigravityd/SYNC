(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules screen ssh)

(operating-system
 (host-name "CUSP")
 (timezone "America/Chicago")
 (locale "en_US.utf8")

 (keyboard-layout (keyboard-layout "us" #:options '("ctrl:swapcaps" "compose:menu")))

 (bootloader (bootloader-configuration
	      (bootloader grub-efi-bootloader)
	      (targets '("/boot/efi"))
	      (keyboard-layout keyboard-layout)))

 (file-systems (cons (file-system
		      (device (file-system-label "/DEV/SDA1")) ;EDIT
		      (mount-point "/")
		      (type "btrfs"))
		     %base-file-systems))

 (users (cons (user-account
	       (name "dnw")
	       (group "users")
	       (supplementary-groups '("wheel" "audio" "video")))
	      %base-user-accounts))

 (packages (cons screen %base-packages))

 (services (append (list (service dhcp-client-service-type)
			 (service openssh-service-type
				  (openssh-configuration
				   (openssh openssh-sans-x)
				   (port-number 2222))))
		   %base-services))
 )
