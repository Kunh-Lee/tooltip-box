;;; tooltip-box.el --- Tooltip Box -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup tooltip-box nil
  "Customization group for the `tooltip-box' package."
  :group 'help
  :group 'mouse
  :group 'tools
  :tag "Tool Tips Box")


;;; Switching tooltips box on/off

(define-minor-mode tooltip-box-mode
  "Toggle Tooltip-Box mode.

When this global minor mode is enabled, Emacs displays help
text (e.g. for buttons and menu items that you put the mouse on)
in a pop-up frame.

When Tooltip-Box mode is disabled, Emacs displays help text in the
echo area."
  :global t
  ;; Even if we start on a text-only terminal, make this non-nil by
  ;; default because we can open a graphical frame later (multi-tty).
  :init-value t
  ;; :initialize 'custom-initialize-delay
  :group 'tooltip-box
  (when tooltip-box-mode
    (add-hook 'pre-command-hook 'tooltip-box-hide)
	(add-hook 'tooltip-box-functions 'tooltip-box-help-tips)
    (add-hook 'x-pre-popup-menu-hook 'tooltip-box-hide))
  (setq show-help-function
	    (when tooltip-box-mode 'tooltip-box-show-help)))

;;; Curstomizable settings

(defcustom tooltip-box-delay 0.7
  "Seconds to wait before displaying a tooltip box the first time."
  :type 'number)

(defcustom tooltip-box-short-delay 0.1
  "Seconds to wait between subsequent tooltips box on different items."
  :type 'number)

(defcustom tooltip-box-recent-seconds 1
  "Display tooltips box if changing tip items within this many seconds.
Do so after `tooltip-box-short-delay'."
  :type 'number)

(defcustom tooltip-box-hide-delay 10
  "Hide tooltips box automatically after this many seconds."
  :type 'number)

(defcustom tooltip-box-padding 10
  "Tooltip box padding."
  :type 'number)

(defcustom tooltip-box-border 1
  "Tooltip box border."
  :type 'number)

(defcustom tooltip-box-font
  (font-spec :name "Sans Serif" :size 10.0)  ;; "Noto Sans-10"
  "Tooltip box font."
  :type 'font-spec)

(defcustom tooltip-box--frame-parameters
  '((tooltip-box-title . t)
    (fullscreen . nil)
    (no-accept-focus . t)
    (no-focus-on-map . nil)
    (width . 0)
    (height . 0)
    (min-width . 0)
    (min-height . 0)
    (max-height . 1000)
    (max-width . 1000)
    (border-width . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (line-spacing . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (cursor-type . nil)
    (minibuffer . nil)
    (no-special-glyphs . t)
    (skip-taskbar . t)
    (inhibit-double-buffering . nil)
    (frame-resize-pixelwise . t)
    (desktop-dont-save . t))
  "Frame parameters used for tooltips box.

Note that font and color parameters are ignored, and the attributes
of the `tooltip-box' face are used instead."
  :type '(repeat (cons :format "%v"
		               (symbol :tag "Parameter")
                       (sexp :tag "Value"))))

(defcustom tooltip-box--buffer-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
	(cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . nil)
    (right-fringe-width . nil)
    (left-margin-width . nil)
    (right-margin-width . nil)
    (fringes-outside-margins . 0)
    (fringe-indicator-alist . nil)
    (indicate-empty-lines . nil)
    (indicate-buffer-boundaries . nil)
    (pixel-scroll-precision-mode . t)
    (buffer-read-only . t))
  "Default child frame buffer parameters."
  :type '(repeat (cons :format "%v"
		               (symbol :tag "Parameter")
                       (sexp :tag "Value"))))

(defface tooltip-box
  '((default :inherit tooltip))
  "Face for Tooltip Box."
  :group 'tooltip-box
  :group 'basic-faces)

;;; Variables that are not customizable.

(defvar tooltip-box-functions nil
  "Functions to call to display tooltips.
Each function is called with one argument EVENT which is a copy
of the last mouse movement event that occurred.  If one of these
functions displays the tooltip, it should return non-nil and the
rest are not called.")

(defvar tooltip-box-timeout-id nil
  "The id of the timeout started when Emacs becomes idle.")

(defvar tooltip-box-last-mouse-motion-event nil
  "A copy of the last mouse motion event seen.")

(defvar tooltip-box-hide-time nil
  "Time when the last tooltip box was hidden.")

(defvar tooltip-box--scroll-pixel nil
  "Whether tooltip box scroll pixel.")

(defvar tooltip-box--frame nil
  "The frame to show tooltip.")

(defvar tooltip-box--frame-font-height nil
  "The font height of tooltip box.")

(defvar tooltip-box--frame-insert nil
  "The num of line to insert of tooltip  box.")

(defvar tooltip-box--temp-pixel nil
  "The pixel gap of tooltip box.")


;;; Timeout for tooltip box display

(defun tooltip-box-delay ()
  "Return the delay in seconds for the next tooltip."
  (if (and tooltip-box-hide-time
	       (time-less-p (time-since tooltip-box-hide-time)
			            tooltip-box-recent-seconds))
      tooltip-box-short-delay
    tooltip-box-delay))

(defun tooltip-box-cancel-delayed-tip ()
  "Disable the tooltip box timeout."
  (when tooltip-box-timeout-id
    (disable-timeout tooltip-box-timeout-id)
    (setq tooltip-box-timeout-id nil)))

(defun tooltip-box-start-delayed-tip ()
  "Add a one-shot timeout to call function `tooltip-box-timeout'."
  (setq tooltip-box-timeout-id
	    (add-timeout (tooltip-box-delay) 'tooltip-box-timeout nil)))

(defun tooltip-box-timeout (_object)
  "Function called when timer with id `tooltip-box-timeout-id' fires."
  (run-hook-with-args-until-success 'tooltip-box-functions
				                    tooltip-box-last-mouse-motion-event))
(defun tooltip-box-hide-tip ()
  "Hide tooltip box."
  (interactive)
  (when (and tooltip-box--frame (frame-live-p tooltip-box--frame))
    (make-frame-invisible tooltip-box--frame)
    (when tooltip-box--scroll-pixel
      (setq tooltip-box--scroll-pixel nil)
      (with-selected-frame tooltip-box--frame
        (pixel-scroll-precision-scroll-up (+ tooltip-box-padding tooltip-box--temp-pixel))))))


(defun tooltip-box--create-frame (buffer)
  "Create and return a child frame with BUFFER."
  (let* ((position (cdr (mouse-pixel-position)))
		 (parent-frame (window-frame (selected-window)))
         (frame-font-height 0))
    (setq tooltip-box--frame
	      (make-frame
	       `(
		     (foreground-color . ,(face-attribute 'tooltip :foreground))
		     (background-color . ,(face-attribute 'tooltip :background))
             ,(cons 'font tooltip-box-font)
		     ;; (child-frame-border . ,(face-attribute 'tooltip-box :box))
		     (parent-frame . ,parent-frame)
             ;; (keep-ratio ,keep-ratio)
             (min-height . 0)
             ,(cons 'left-fringe tooltip-box-padding)
             ,(cons 'right-fringe tooltip-box-padding)
             (child-frame-border-width . ,tooltip-box-border)
		     ,@tooltip-box--frame-parameters)))
    (set-face-attribute 'fringe tooltip-box--frame :background 'unspecified :inherit 'tooltip-box)
    (set-face-background 'child-frame-border (face-attribute 'tooltip :foreground) tooltip-box--frame)
    (set-window-buffer (frame-root-window tooltip-box--frame) buffer)
    (set-window-dedicated-p (frame-root-window tooltip-box--frame) t)
    (with-selected-frame tooltip-box--frame
      (setq frame-font-height (default-font-height))
      (setq tooltip-box--frame-font-height (default-font-height))
      (setq tooltip-box--frame-insert (ceiling (/ (* 2 tooltip-box-padding) (float frame-font-height))))
      (setq tooltip-box--temp-pixel (-
                                     (* tooltip-box--frame-insert frame-font-height)
                                     tooltip-box-padding tooltip-box-padding)))))

(defun tooltip-box--make-buffer ()
  "Create buffer with NAME."
  (let ((buffer (get-buffer-create " *tooltip-box*")))
    (with-current-buffer buffer
      ;;; XXX HACK install mouse ignore map
      (dolist (var tooltip-box--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var))))
    (tooltip-box--create-frame buffer)))

(defun tooltip-box-show (msg)
  "Create a tooltip box of MSG."
  (let* ((buffer (get-buffer-create " *tooltip-box*")))
    (with-current-buffer buffer
	  (with-silent-modifications
	    (erase-buffer)
        (insert (format "%s%s" (make-string tooltip-box--frame-insert ?\n) msg))))
    ;; To get the right window-text-pixel-size.
    (set-frame-size tooltip-box--frame 1000 1000 t)
    (let* ((window (frame-selected-window tooltip-box--frame))
           (size (window-text-pixel-size window nil nil nil nil t))
           (left (cadr (mouse-pixel-position)))
           (top (cddr (mouse-pixel-position)))
           (width (car size))
           (height (cdr size))
           (frame-resize-pixelwise t))
      ;; (message (format "font-height:%s  width:%s  height:%s  size:%s  padding:%s  window:%s"
      ;;                  tooltip-box--frame-font-height width height size tooltip-box-padding window))
      (unless tooltip-box--scroll-pixel
        (setq tooltip-box--scroll-pixel t
              height (- height tooltip-box--temp-pixel))
        (with-selected-frame tooltip-box--frame
          (pixel-scroll-precision-scroll-down (+ tooltip-box--temp-pixel tooltip-box-padding))))
      (if (> (+ left width tooltip-box-padding
                tooltip-box-padding tooltip-box-border tooltip-box-border)
             (frame-pixel-width))
          (set-frame-parameter tooltip-box--frame
                               'left (- left width tooltip-x-offset
                                        tooltip-box-padding tooltip-box-padding))
        (set-frame-parameter tooltip-box--frame 'left (+ left tooltip-x-offset)))
      (if (> (+ top height tooltip-box-border tooltip-box-border tooltip-y-offset)
             (frame-pixel-height))
          (set-frame-parameter tooltip-box--frame
                               'top (- top height tooltip-y-offset))
        (set-frame-parameter tooltip-box--frame 'top (+ top tooltip-y-offset)))
      (set-frame-size tooltip-box--frame width height t)
      (make-frame-visible tooltip-box--frame))))

(defun tooltip-box-hide (&optional _ignored-arg)
  "Hide a tooltip box, if one is displayed.
Value is non-nil if tooltip box was open."
  (tooltip-box-cancel-delayed-tip)
  (when (tooltip-box-hide-tip)
    (setq tooltip-box-hide-time (float-time))))

;;; Tooltip Box help.

(defvar tooltip-box-help-message nil
  "The last help message received via `show-help-function'.
This is used by `tooltip-box-show-help'")

(defun tooltip-box-show-help (msg)
  "Function installed as `show-help-function'.
MSG is either a help string to display, or nil to cancel the display."
  (if (and (display-graphic-p)
           ;; Tooltips Box can't be displayed on top of the global menu
           ;; bar on NS.
           (or (not (eq window-system 'ns))
               (not (menu-or-popup-active-p))))
      (let ((previous-help tooltip-box-help-message))
	    (setq tooltip-box-help-message msg)
	    (cond ((null msg)
	           ;; Cancel display.  This also cancels a delayed tip, if
	           ;; there is one.
	           (tooltip-box-hide))
	          ((equal previous-help msg)
	           ;; Same help as before (but possibly the mouse has
	           ;; moved or the text properties have changed).  Keep
	           ;; what we have.  If only text properties have changed,
	           ;; the tooltip won't be updated, but that shouldn't
	           ;; occur.
	           )
	          (t
	           ;; A different help.  Remove a previous tooltip, and
	           ;; display a new one, with some delay.
	           (tooltip-box-hide)
	           (tooltip-box-start-delayed-tip)
               )))))

(defun tooltip-box-help-tips (_event)
  "Hook function to display a help tooltip box.
This is installed on the hook `tooltip-functions', which
is run when the timer with id `tooltip-timeout-id' fires.
Value is non-nil if this function handled the tip."
  (when (stringp tooltip-box-help-message)
    (tooltip-box-show tooltip-box-help-message)
    t))

(tooltip-box--make-buffer)
(tooltip-box-mode)

(provide 'tooltip-box)
;;; tooltip-box.el ends here
