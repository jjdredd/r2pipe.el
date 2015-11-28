(load-file "r2pipe.el")

(setq r2p (r2-pipe-new "/bin/ls"))
(r2-cmd r2p "e scr.color = false")
(print (r2-cmd r2p "pd 100"))
(r2-pipe-close r2p)
