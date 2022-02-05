#lang rosette

(provide (all-defined-out))


(struct message (recipient sender value data gas) #:transparent)
(struct transaction (internal message cmds) #:transparent)
