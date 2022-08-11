;; Prevent package.el loading packages prior to their init-file loading. 
(setq package-enable-at-startup nil)
(setq crafted-package-system 'straight)
(crafted-package-bootstrap crafted-package-system)
