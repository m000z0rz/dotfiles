
(require 'magit)
(require 'forge)

;; TODO
;; * more icons instead of status string?
;; * when running, automatically refresh periodically?
;; * test hot job and failed job
;; * section keymaps to visit things, particularly failed job reports
;;   * links, and wslview
;; * add commands to easily add/remove pipelines to watch

;; (magit-section-value-if <predicate>)
;; (word-at-point)
;; TODO: instead of "b", interactive shoudl take function to identify the thing using stuff like above
;;   will also requires sections
(defun magit-pipelines-visit-thing (url)
	(interactive (list (thing-at-point 'url)))
	(when url
		(message (format "Opening with browse-url-generic: %s" url))
		(browse-url-generic url)))

(defvar magit-pipelines-test-section-map
	(let ((map (make-sparse-keymap)))
		(define-key map [remap magit-visit-thing] 'magit-pipelines-visit-thing)
		map))

(defclass pipeline-section (magit-section)
	((keymap :initform 'magit-pipelines-test-section-map)))

(defvar magit-pipelines-result nil
	"Cached pipeline data")

(defun magit-pipelines--regroup-alist (alist keyfun valuefun)
	(seq-filter
	 (lambda (kvp) ;; filter nil keys
		 (car kvp))
	 (mapcar (lambda (kvp) ;; new cons pairs by applying keyfun and valuefun to values
						 (let ((value (cdr kvp)))
							 (cons (funcall keyfun value) (funcall valuefun value))))
					 alist)))

(defun magit-pipelines--results-by-ref ()
	(magit-pipelines--regroup-alist

	 (nalist-get '(data project)
							 ;;TODO: have nalist-get use alist-get with equal for string?
							 (cdr (assoc (magit-pipelines--get-forge-url) magit-pipelines-result)))
	 ;; keyfun: the ref of the first node
	 (lambda (val)
		 (nalist-get '(nodes [0] ref) val))
	 ;; valuefun: the first node
	 (lambda (val)
		 (nalist-get '(nodes [0]) val))))

(defun magit-pipelines--insert-pipelines ()
	(let ((repo (forge-get-repository nil)))
		(when (forge-gitlab-repository-p repo)
			(magit-insert-section (pipeline-section)
				(magit-insert-heading
					(magit--propertize-face "Pipelines"
																	'magit-section-heading))
				(magit-insert-section-body
					(mapc 'magit-pipelines--insert-pipeline-status (magit-pipelines--results-by-ref)))))))

(defun magit-pipelines--project-full-path (repo)
	(concat (oref repo owner) "/" (oref repo name)))

(defun magit-pipelines--get-forge-url ()
	(magit-git-string "remote" "get-url" (forge--get-remote)))

(defun magit-pipelines--ref-display (ref)
	(let ((mr-matcher "^refs/merge-requests/\\([0-9]+\\)/head$"))
		(if (string-match mr-matcher ref)
				(concat
				 "!"
				 (match-string 1 ref))
			ref)))

(defun magit-pipelines--insert-pipeline-status (def)
	(let ((ref (car def))
				(node (cdr def)))

		(insert (magit-pipelines--ref-display ref))
		(insert ": ")
		(pcase (alist-get 'status node)
			("SUCCESS" (insert (concat
													(magit--propertize-face "✓ " 'success) ;; 'magit-bisect-good ?
													(apply 'format "%i%c" (magit--age
																								 (magit-pipelines--iso8601-to-unix-timestamp (alist-get 'finishedAt node))
																								 t))
													" ago by "
													(nalist-get '(user username) node)
													" - "
													(nalist-get '(commit title) node))))
			("RUNNING" (let ((hot-job (magit-pipelines--hot-job node)))
									 (insert (concat
														(magit--propertize-face "҉ " 'warning)
														"Running... "
														(alist-get 'name hot-job)
														" "
														(alist-get 'status hot-job)
														" "
														(nalist-get '(user username) node)
														" - "
														(nalist-get '(commit title) node)
														))))
			("FAILED" (let ((repo (forge-get-repository nil))
											(first-failed (nalist-get '(failedJobs nodes [0]) node)))

									(insert (concat
													 (magit--propertize-face "✗ " 'error) ;; 'magit-bisect-bad ?
													 (magit--propertize-face
														(alist-get 'name first-failed)
														'magit-bisect-bad)
													 " "
													 (concat
														"https://"
														(oref repo forge)
														(nalist-get '(detailedStatus detailsPath) first-failed))))))
			(_ (insert (concat
									(alist-get 'status node)
									" "
									(nalist-get '(user username) node)
									" - "
									(nalist-get '(commit title) node))))))
	(insert "\n"))

(defun magit-pipelines--query-for-refs (full-path refs)
	`(query
		(project [(fullPath ,full-path)]
						 ,@(mapcar 'magit-pipelines--query-for-ref refs)
						 )))

(defun magit-pipelines--iso8601-to-unix-timestamp (datestr)
	(time-convert (encode-time (iso8601-parse datestr)) 'integer))

(defun nalist-get (path alist)
	"nested alist get
  fn (NODE1 NODE2 [INDEX3] ... NODEN) ALIST"
	(pcase path
		((pred null) alist)
		(`([,index] . ,rest) (nalist-get rest (nth index alist)))
		(`(,head . ,rest) (nalist-get rest (alist-get head alist)))
		(thing (user-error (format "nalist-get unrecognized path %s" thing )))))

;;(nalist-get '(friend) '((nodes foo bar baz) (friend . you)))

;; (iso8601-parse "2022-08-31T09:24:24-05:00")
;; (encode-time (iso8601-parse "2022-08-31T09:24:24-05:00"))
;; (time-convert (iso8601-parse "2022-08-31T09:24:24-05:00") 'integer)
;; (time-convert (encode-time (iso8601-parse "2022-08-31T09:24:24-05:00")) 'integer)
;; (format "%i" (magit-pipelines--iso8601-to-unix-timestamp  "2022-08-31T09:24:24-05:00"))
;;(magit--age (time-convert (encode-time (iso8601-parse "2022-08-31T09:24:24-05:00")) 'integer) t)
;; (time-convert nil 'integer)
;; (float-time)

(defun magit-pipelines--query-for-ref (ref)
	`((,(concat "pipeline_" (magit-pipelines--safe-ref ref)) pipelines) [(ref ,ref)
																																			 (first 1)]
	 	,magit-pipelines--query-pipeline-fields))

(defun magit-pipelines--safe-ref (ref)
	(s-replace "-" "_"
						 (s-replace "/" "_" ref)))

(defun magit-pipelines--hot-job (pipeline-node)
	"Gets the first job for a pipeline that isn't waiting on another job and isn't yet done"
	(seq-find (lambda (job-node)
							(member (alist-get 'status job-node) '("WAITING_FOR_RESOURCE" "PREPARING" "PENDING" "RUNNING")))
						(nalist-get '(jobs nodes) pipeline-node)))

(defconst magit-pipelines--query-pipeline-fields
	'(nodes
		ref
		status
		(detailedStatus
		 label
		 hasDetails
		 detailsPath)
		(commit
		 sha
		 title)
		startedAt
		finishedAt
		(user
		 username
		 name)
		((failedJobs jobs) [(statuses FAILED)]
		 (nodes
			name
			(detailedStatus
			 label
			 hasDetails
			 detailsPath)))
		(jobs
		 (nodes
			name
			status
			startedAt
			finishedAt))))

(defun magit-pipelines--get-refs ()
	(let* ((current (magit-git-string "branch" "--show-current"))
				 (listed (magit-get-all "magit-pipelines.list"))
				 (current-mr (magit-pipelines--merge-request-branch (magit-pipelines--get-open-merge-request-id current))))
		(remove nil `(,@listed ,(unless (member current listed) current) ,current-mr))))

;; ask sean: why did i have to manually do the closure for prev-result?
;;  closures work with othe trivial examples. must be something to do with the asynchronoicty of ghub-graphql?
(defun magit-pipelines-fetch (&optional was-called-interactively)
	(interactive "p") ;; numeric prefix argument is never nil when called interactively
	(let ((repo (forge-get-repository nil)))
		(if (forge-gitlab-repository-p repo)
				(let* ((query (magit-pipelines--query-for-refs (magit-pipelines--project-full-path repo) (magit-pipelines--get-refs)))
							 (remote-url (magit-pipelines--get-forge-url))
							 (prev-result (cdr (assoc remote-url magit-pipelines-result)))
							 (cb `(lambda (result _headers _status _req)
											(let ((remote-url-inner (magit-pipelines--get-forge-url)))
												(setq magit-pipelines-result
															(assoc-delete-all remote-url-inner magit-pipelines-result))
												(add-to-list 'magit-pipelines-result (cons remote-url-inner result))
												(unless (equal result ',prev-result)
													(magit-refresh))))))
					(ghub-graphql query nil
												:forge 'gitlab
												:host (oref repo apihost)
												:auth 'forge
												:callback cb))
			(when was-called-interactively
				(user-error "Could not identify gitlab repo")))))

(defun magit-pipelines--on-refresh-buffer ()
	(when (derived-mode-p 'magit-status-mode)
		(let ((repo (forge-get-repository nil)))
			(when (forge-gitlab-repository-p repo)
				(magit-pipelines-fetch)))))

;; getting mr from ref...
;; (forge-sql [:select * :from $i1
;; :where (and (= repository $s2)
;;             (isnull closed))
;; :order-by [(desc updated)]
;; :limit $s3]
;; table id open-limit)
;; (forge-sql [:select * :from $i1
;;                     :where (and (= repository $s2)
;;                                 (isnull closed))]

;; get number?
;; select number from pullreq where head_ref="dlg/123456" and state="open"
;; then merge request branch is refs/merge-requests/$iid/head
;; confirmed this works: pipeline_dev: pipelines (ref: "refs/merge-requests/123/head") {

;; repo id is (oref repo id)
;;

(defun magit-pipelines--get-open-merge-request-id (for-ref)
	(let ((repo (forge-get-repository nil)))
		(car (car (forge-sql [:select [number] :from pullreq
																	:where (and (= repository $s1)
																							(= head_ref $s2)
																							(= state 'open))
																	:limit 3]
												 (oref repo id) for-ref)))))

(defun magit-pipelines--merge-request-branch (merge-request-id)
	(when merge-request-id
		(format "refs/merge-requests/%i/head" merge-request-id)))

(magit-add-section-hook 'magit-status-sections-hook #'magit-pipelines--insert-pipelines nil t)
(add-hook 'magit-refresh-buffer-hook 'magit-pipelines--on-refresh-buffer)
