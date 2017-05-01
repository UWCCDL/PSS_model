;;; ------------------------------------------------------------------
;;; AN ACT-R MODEL OF THE PROBABILISTIC STIMULUS SELECTION (PSS) TASK 
;;; ------------------------------------------------------------------
;;;
;;; This is a "canonical" implementation of the PSS task in ACT-R.
;;; but the learning algorithm is changed so that all the productions
;;; competing for an action receive reward.
;;;
;;; ------------------------------------------------------------------
;;; (c) 2017, Andrea Stocco,
;;;     University of Washington
;;;     stocco@uw.edu
;;; ------------------------------------------------------------------
;;; 

(clear-all)

(define-model pss-model-non-competitive-special

(sgp :er t
     :auto-attend t
     :overstuff-visual-location t
     :esc t
     :ul t
     ;;:reward-hook bg-reward-hook
     :reward-hook bg-reward-hook-selection
     :alpha 0.1
     :egs 0.1
;     :model-warnings nil
     :style-warnings nil)
  ;;:utility-hook bg-utility-hook)

;(sgp :trace-filter production-firing-only)

(set-visloc-default screen-x 200 :attended new)

(chunk-type pss-task step)

(chunk-type working-memory wm shape-a shape-b shape-c shape-d shape-e shape-f left right)

(chunk-type (pss-visual-object (:include visual-object))
	    what shape color position)

(chunk-type (pss-visual-location (:include visual-location))
	    shape color position)


(add-dm (wait isa chunk)
	(choice isa chunk)
	(respond isa chunk)
	(attending isa chunk)
	(feedback isa chunk)
	(decision isa chunk)

        ; Visual information
	(screen isa chunk)
	(choices isa chunk)
	(option isa chunk)
	(present isa chunk)
	(correct isa chunk)
	(incorrect isa chunk)
	(done isa chunk)
			  
	; Center screen positions (left and right already exist)
	(center isa chunk)
	

	; The stimuli
	(shape-A isa chunk)
	(shape-B isa chunk)
	(shape-C isa chunk)
	(shape-D isa chunk)
	(shape-E isa chunk)
	(shape-F isa chunk)
	(do-pss isa pss-task step wait))

;; ---------------------------------------------------------------- ;;
;; Perceptual processes and task interaction
;; ---------------------------------------------------------------- ;;

(p just-in-case
   "In case there is something unattended on the screen, attend to it"
   =visual-location>
      kind screen
      value choices
      ;attended nil
     
   ?visual>
      state free
      buffer empty
      
   ?imaginal>
      state free
      buffer empty
==>
   +visual>
      isa move-attention
      screen-pos  =visual-location
) 

(p begin-binary-decision
   "Start the decision-process, preparing WM structures to compare alternatives "
   =goal>
;    - step respond
    - step nil
   
   ?visual>
      state free 

   =visual>
      what screen
      value choices

   ?manual>
      preparation free
      processor free
      execution free
      
   ?imaginal>
      buffer empty
      state free
==>
   +imaginal>
      wm decision

   =goal>
      step choice

   =visual>
)

(p attend-option-left
   "Attend the left option"
   =goal>
      step choice
   
   =imaginal>
      left nil

    ?visual>
      state free

==>
   =goal>
      step attending
   
   +visual-location>
      kind option
      position left
      ;attended nil
   =imaginal>
)

(p attend-option-right
   "Attend th right option"
   =goal>
      step choice
   
   =imaginal>
      right nil
   
   ?visual>
      state free   
==> 
   =goal>
      step attending
   +visual-location>
      kind option
      position right
      ;attended nil
   =imaginal>
)

(p encode-option
   "Encode in WM whatever option we are attending"
   =goal>
      step attending

   =visual>
      what option
      shape =SHAPE
      position =POSITION
      
   =imaginal>
      =POSITION nil
==>
   =goal>
      step choice
   =imaginal>
      =POSITION =SHAPE
      =SHAPE present
)

;; ---------------------------------------------------------------- ;;
;; Decision processes
;; ---------------------------------------------------------------- ;;

(p pick-shape-A
   =goal>
      step choice
   
   =imaginal>
      - left nil    ; This is only to make sure the options are available
      - right nil  ; This is only to make sure the options are available
      shape-A present

==>
   =goal>
      step respond
   
   +visual-location>
      kind option
      value shape-A

   -imaginal>
)


(p pick-shape-B
   =goal>
      step choice
   
   =imaginal>
      - left nil    ; This is only to make sure the options are available
      - right nil  ; This is only to make sure the options are available
      shape-b present

==>
   =goal>
      step respond
   
   +visual-location>
      kind option
      value shape-B

   -imaginal>

)


(p pick-shape-C
   =goal>
      step choice
   
   =imaginal>
      - left nil    ; This is only to make sure the options are available
      - right nil  ; This is only to make sure the options are available
      shape-C present

==>
   =goal>
      step respond
   
   +visual-location>
      kind option
      value shape-C

   -imaginal>
)

(p pick-shape-D
   =goal>
      step choice
   
   =imaginal>
      - left nil    ; This is only to make sure the options are available
      - right nil  ; This is only to make sure the options are available
      shape-D present

==>
   =goal>
      step respond
   
   +visual-location>
      kind option
      value shape-D

   -imaginal>
)

(p pick-shape-E
   =goal>
      step choice
   
   =imaginal>
      - left nil    ; This is only to make sure the options are available
      - right nil  ; This is only to make sure the options are available
      shape-E present

==>
   =goal>
      step respond
   
   +visual-location>
      kind option
      value shape-E

   -imaginal>
)

(p pick-shape-F
   =goal>
      step choice
   
   =imaginal>
      - left nil    ; This is only to make sure the options are available
      - right nil  ; This is only to make sure the options are available
      shape-F present

==>
   =goal>
      step respond
   
   +visual-location>
      kind option
      value shape-F

   -imaginal>
)

;; ---------------------------------------------------------------- ;;
;; Motor response processes.
;; ---------------------------------------------------------------- ;;
      

(p respond
   "Press the button on the same side of the stimulus when we are ready to respond"
   =goal>
      step respond
   
   =visual>
      what option
      position =SIDE

   ?manual>
      preparation free
      processor free
      execution free
      
   ?imaginal>
      state free
      buffer empty
==>
   +manual>
      isa punch
      hand =SIDE
      finger index
      
   +visual-location>
      position center
   

)

;; ---------------------------------------------------------------- ;;
;; Process feedback. And waits for more screen changes.
;; ---------------------------------------------------------------- ;;


(p process-feedback-correct
   "Processes feedback on the screen"
   =goal>
    - step wait 
   
   ?visual>
      state free 

   =visual>
      what feedback
      value correct
==>
   =goal>
      step wait
   =visual>
      
;   !eval! (process-reward)   
)

(p process-feedback-incorrect
   "Processes feedback on the screen"
   =goal>
    - step wait 
   
   ?visual>
      state free 

   =visual>
      what feedback
      value incorrect
==>
   =goal>
      step wait
   =visual>
      
;   !eval! (process-reward)   
)


;; ---------------------------------------------------------------- ;;
;; DONE
;; ---------------------------------------------------------------- ;;

(p done
   "Stop when done"
   =visual>
      what done
==>
   !stop!   
)      

(goal-focus do-pss)

(spp-fct `(process-feedback-correct :reward ,*positive-reward*))
(spp-fct `(process-feedback-incorrect :reward ,*negative-reward*))


)
