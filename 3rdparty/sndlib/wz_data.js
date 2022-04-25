var extsnd_addmark_tip = "<code>(add-mark samp snd chn name (sync 0))</code>:<br>" +  
                         " add a mark at sample 'samp' returning the mark id.";

var extsnd_addsoundfileextension_tip = "<code>(add-sound-file-extension ext)</code>:<br>" +
                                       " add the file extension 'ext' to the list of sound file extensions";

var extsnd_addtomenu_tip = "<code>(add-to-menu menu label func position)</code>:<br>" +
                           " add label to menu (a main menu index),<br>" +
                           " invoke func (a function of no args) when the new menu is activated.<br>" +
                           " Return the new menu label widget.";

var extsnd_addtransform_tip = "<code>(add-transform name x-label low high func)</code>:<br>" +
                              " add the transform func to the transform lists;<br>" +
                              " func should be a function of two arguments, <br>" +
                              " the length of the transform and a sampler to get the data, <br>" +
                              " and should return a vct containing the transform results. <br>" +
                              " name is the transform's name, x-label is its x-axis label, <br>" +
                              " and the relevant returned data to be displayed goes from low to high (normally 0.0 to 1.0)";

var extsnd_afterapplycontrolshook_tip = "<code>after-apply-controls-hook (snd)</code>: called when apply-controls finishes.";

var extsnd_aftergraphhook_tip = "<code>after-graph-hook (snd chn)</code>: called after a graph is updated.";

var extsnd_afteropenhook_tip = "<code>after-open-hook (snd)</code>: called just before the new file's window is displayed.<br>" +
                               " This provides a way to set various sound-specific defaults. <pre>" +
                               "  (hook-push after-open-hook<br>" + 
                               "    (lambda (snd) <br>" +
                               "      (if (> (channels snd) 1) <br>" +
                               "          (set! (channel-style snd) channels-combined))))</pre>";

var extsnd_aftersaveashook_tip = "<code>after-save-as-hook (saved-sound-index save-as-full-filename from-save-as-dialog)</code>:<br>" +
                                 " called upon File:Save as or save-sound-as completion.";

var extsnd_aftersavestatehook_tip = "<code>after-save-state-hook (filename)</code>: called after Snd state has been saved;<br>" +
                                    " filename is the save state file.";

var extsnd_aftertransformhook_tip = "<code>after-transform-hook (snd chn scaler)</code>: called just after a spectrum is calculated.";

var extsnd_ampcontrol_tip = "<code>(amp-control snd chn)</code>: current amp slider setting";

var extsnd_applycontrols_tip = "<code>(apply-controls snd (choice 0) (beg 0) (dur len))</code>:<br>" +
                               " applies the current control panel state as an edit. <br>" +
                               " The 'choices' are 0 (apply to sound), 1 (apply to channel), and 2 (apply to selection).<br>" +
                               " If 'beg' is given, the apply starts there.";

var extsnd_asoneedit_tip = "<code>(as-one-edit thunk origin)</code>: evaluate thunk,<br>" +
                           " collecting all edits into one from the edit history's point of view";

var extsnd_axisinfo_tip = "<code>(axis-info snd chn (ax time-graph))</code>: info about axis:<br>" +
                          "<pre> (list losamp hisamp x0 y0 x1 y1 xmin ymin xmax ymax pix_x0 pix_y0 pix_x1 pix_y1<br>" +
                          "       y_offset xscale yscale xlabel ylabel new-peaks)</pre>";

var extsnd_axislabelfont_tip = "<code>(axis-label-font)</code>: font used for axis labels";

var extsnd_axisnumbersfont_tip = "<code>(axis-numbers-font)</code>: font used for axis numbers";

var extsnd_badheaderhook_tip = "<code>bad-header-hook (filename)</code>: called if a file has some bogus-looking header.<br>" +
                               " Return #t to give up on that file.";

var extsnd_basiccolor_tip = "<code>(basic-color)</code>: Snd's basic color";

var extsnd_beatsperminute_tip = "<code>(beats-per-minute snd chn)</code>: beats per minute if x-axis-style is x-axis-in-beats";

var extsnd_beforeclosehook_tip = "<code>before-close-hook (snd)</code>: called each time a file is closed (before the close).<br>" +
                                 " If it returns #t, the file is not closed.";

var extsnd_beforeexithook_tip = "<code>before-exit-hook ()</code>: called upon exit. If it returns #t, Snd does not exit.<br>" +
                                " This can be used to check for unsaved edits.";

var extsnd_beforesaveashook_tip = "<code>before-save-as-hook (index filename selection srate type format comment)</code>:<br>" +
                                  " called before File:Save as or save-sound-as. Provides a way to fixup a sound just before it is saved.";

var extsnd_beforesavestatehook_tip = "<code>before-save-state-hook (filename)</code>: called before Snd state is saved.<br>" +
                                     " If the hook functions return #t, the save state process opens 'filename' for appending, rather than truncating.";

var extsnd_besj0_tip = "<code>(bes-j0 x) returns the Bessel function J0(x)";

var extsnd_bindkey_tip = "<code>(bind-key key modifiers func extended origin prefs-info)</code>:<br>" +
                         " causes 'key' (an integer, character, or string) when typed with 'modifiers'<br>" +
                         " (0:none, 4:control, 8:meta) (and C-x if extended) to invoke 'func', a function of zero or one arguments.<br>" +
                         " If the function takes one argument, it is passed the preceding C-u number, if any.<br>" +
                         " The function should return one of the cursor choices (e.g. keyboard-no-action).<br>" +
                         "  'origin' is the name reported if an error occurs.<br>" +
                         " The 'key' argument can be the X/Gtk name of the key (e.g. \"plus\" for \"+\" or \"Home\"),<br>" +
                         " the character on the key (#\x07), or the integer corresponding to that character:<br>" +
                         " (\"(char-&gt;integer #\x07)\" in Scheme, or \"?a\" in Ruby.";

var extsnd_boldpeaksfont_tip = "<code>(bold-peaks-font)</code>: bold font used by fft peak display";

var extsnd_channels_tip = "<code>(channels snd)</code>: how many channels snd has";

var extsnd_channelstyle_tip = "<code>(channel-style snd)</code>: how multichannel sounds lay out the channels.<br>" +
                              " The default is channels-combined; other values are channels-separate and channels-superimposed.<br>" +
                              " As a global (if the 'snd' arg is omitted), it is the default setting for each sound's 'unite' button.";

var extsnd_channeltovct_tip = "<code>(channel-&gt;vct beg dur snd chn edpos)</code>: return a vct with the specified samples.";

var extsnd_channelwidgets_tip = "<code>(channel-widgets snd chn)</code>: a list of widgets: ((0)graph (1)w (2)f (3)sx (4)sy (5)zx (6)zy (7)edhist)";

var extsnd_chans_tip = "<code>(channels snd)</code>: how many channels snd has";

var extsnd_cliphook_tip = "<code>clip-hook (clipping-value)</code> is called each time a sample is about to be clipped<br>" +
                          " upon being written to a sound file.  The hook function can return the new value to be written,<br>" +
                          " or rely on the default (-1.0 or 1.0 depending on the sign of 'clipping-value').";

var extsnd_clmchannel_tip = "<code>(clm-channel gen (beg 0) (dur len) snd chn edpos (overlap 0) origin)</code>:<br>" +
                            " apply gen to snd's channel chn starting at beg for dur samples.<br>" +
                            " overlap is the 'ring' time, if any.";

var extsnd_closehook_tip = "<code>close-hook (snd)</code>: called each time a file is closed (before the close).";

var extsnd_closesound_tip = "<code>(close-sound snd)</code>: close snd";

var extsnd_comment_tip = "<code>(comment snd)</code>: snd's comment (in its header)";

var extsnd_cursor_tip = "<code>(cursor snd chn edpos)</code>: current cursor location in snd's channel chn";

var extsnd_cursorcontext_tip = "graphics context for the cursor";

var extsnd_dachook_tip = "<code>dac-hook (sdobj)</code>: called just before data is sent to DAC passing data as sound-data object";

var extsnd_dacsize_tip = "<code>(dac-size)</code>: the current DAC buffer size in frames (256)";

var extsnd_datacolor_tip = "<code>(data-color)</code>: color used to draw unselected data";

var extsnd_dataformat_tip = "<code>(data-format snd)</code>: snd's data format (e.g. mus-bshort)";

var extsnd_datalocation_tip = "<code>(data-location snd)</code>: snd's data location (bytes)";

var extsnd_defineenvelope_tip = "<code>(define-envelope name data)</code>: define 'name' to have the value 'data'<br>" +
                                " (a list of breakpoints), and load it into the envelope editor.";

var extsnd_deletesamples_tip = "<code>(delete-samples start-samp samps snd chn edpos)</code>:<br>" +
                               " delete 'samps' samples from snd's channel chn starting at 'start-samp'";

var extsnd_dialogwidgets_tip = "<code>(dialog-widgets)</code>: dialog widgets (each #f if not yet created)</code>:<br>" +
                               " <code>(list  (0 color-dialog) (1 orientation-dialog) (2 enved-dialog)<br>" +
                               " (3 #f) (4 #f) (5 transform-dialog)  (6 open-file-dialog) (7 save-sound-dialog)<br>" +
                               " (8 view-files-dialog) (9 raw data dialog) (10 new file dialog)<br>" +
                               " (11 mix-file-dialog) (12 edit-header-dialog) (13 find-dialog)<br>" +
                               " (14 help-dialog) (15 listener completion)  (16 view-mixes-dialog)<br>" +
                               " (17 print-dialog) (19 view-regions-dialog)<br>" +
                               " (20 info-dialog) (21 #f) (22 save-selection-dialog)<br>" +
                               " (23 insert-file-dialog)  (24 save-region-dialog) (25 preferences-dialog))</code>";

var extsnd_dotsize_tip = "<code>(dot-size snd chn)</code>: size in pixels of dots when graphing with dots (1)";

var extsnd_drawline_tip = "<code>(draw-line x0 y0 x1 y1 snd chn (ax time-graph))</code>: draw a line";

var extsnd_drawstring_tip = "<code>(draw-string text x0 y0 snd chn (ax time-graph))</code>: draw a string";

var extsnd_duringopenhook_tip = "<code>during-open-hook (fd name reason)</code>:<br>" +
                                " called after file is opened, but before data has been read.";

var extsnd_editfragment_tip = "<code>(edit-fragment (ctr current-edit-position) snd chn)</code>:<br>" +
                              " edit history entry at ctr associated with snd's channel chn;<br>" +
                              " the returned value is a list (origin type start-sample samps)";

var extsnd_editposition_tip = "<code>(edit-position snd chn)</code>: current edit history position in snd's channel chn";

var extsnd_edits_tip = "<code>(edits snd chn)</code>: <br>" +
                       "returns <code>(list undoable-edits redoable-edits)</code> in snd's channel chn";

var extsnd_emarks_tip = "<code>(marks snd chn edpos)</code>: list of marks (ids) in snd/chn<br>" +
                        " at edit history position pos. mark list is: <br>" +
                        " if channel given: (id id ...), <br>" +
                        " if snd given: ((id id) (id id ...)), <br>" +
                        " if neither given: (((id ...) ...) ...).";

var extsnd_envchannel_tip = "<code>(env-channel env-gen-or-envelope (beg 0) (dur len) snd chn edpos)</code>:<br>" +
                            " apply amplitude envelope to snd's channel chn starting at beg for dur samples.";

var extsnd_envedtarget_tip = "<code>(enved-target)</code>: determines how the envelope edit envelope is applied:<br>" +
                             " enved-amplitude, enved-srate (apply to speed), and enved-spectrum (apply as a filter).";

var extsnd_envedwaving_tip = "<code>(enved-wave?)</code>: #t if the envelope editor is displaying the waveform to be edited";

var extsnd_envsound_tip = "<code>(env-sound env (start-samp 0) (samps len) (env-base 1.0) snd chn edpos)</code>:<br>" +
                          " apply an amplitude envelope (a list of breakpoints or a CLM env) to snd's channel chn<br>" +
                          " starting at start-samp, going either to the end of the sound or for samps samples,<br>" +
                          " with segments interpolating according to env-base (1 = linear).";

var extsnd_envselection_tip = "<code>(env-selection env (env-base 1.0))</code>:<br>" +
                              " apply envelope to the selection using env-base to determine how breakpoints are connected";

var extsnd_eregions_tip = "<code>(regions)</code>: current active regions (a list of region ids)";

var extsnd_exit_tip = "<code>(exit)</code>: exit Snd";

var extsnd_exithook_tip = "<code>exit-hook ()</code>: called upon exit.  This can be used to perform cleanup activities.";

var extsnd_expandcontrol_tip = "<code>(expand-control snd)</code>: current expand slider setting";

var extsnd_expandcontrolp_tip = "<code>(expand-control? snd)</code>: snd's control panel expand button state";

var extsnd_filename_tip = "<code>(file-name snd)</code>: snd's full filename";

var extsnd_fillrectangle_tip = "<code>(fill-rectangle x0 y0 width height snd chn (ax time-graph) erase)</code>: draw a filled rectangle";

var extsnd_filterchannel_tip = "<code>(filter-channel env order beg dur snd chn edpos (truncate #t) origin)</code>:<br>" +
                               " applies an FIR filter to snd's channel chn.<br>" +
                               " 'env' is the frequency response envelope, or a vct with the coefficients.";

var extsnd_filterselection_tip = "<code>(filter-selection filter order (truncate #t))</code>:<br>" +
                                 " apply filter to selection.<br>" +
                                 " If truncate, cut off filter output at end of selection, else mix";

var extsnd_filtersound_tip = "<code>(filter-sound filter order snd chn edpos origin)</code>:<br>" +
                             " applies FIR filter to snd's channel chn.<br>" +
                             " 'filter' is either the frequency response envelope,<br>" +
                             " a CLM filter, or a vct with the actual coefficients";

var extsnd_focuswidget_tip = "<code>(focus-widget widget)</code>: cause widget to receive input focus";

var extsnd_frames_tip = "<code>(frames snd chn edpos)</code>: number of frames of data in snd's channel chn";

var extsnd_freesampler_tip = "<code>(free-sampler reader)</code>: free a sampler (of any kind)";

var extsnd_graph_tip = "<code>(graph data xlabel (x0 0.0) (x1 1.0) y0 y1 snd chn (force-display #t) show-axes)</code>:<br>" +
                       " displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1;<br>" +
                       " 'data' can be a list or a vct. If 'data' is a list of numbers, it is treated as an envelope.";

var extsnd_graphhook_tip = "<code>graph-hook (snd chn y0 y1)</code>: called each time a graph is about to be updated. <br>" +
                           "If it returns #t, the display is not updated.";

var extsnd_graphonce_tip = "<code>graph-once</code> is the default value of the graph types (time-graph-type and transform-graph-type).";

var extsnd_graphshorizontal_tip = "<code>(graphs-horizontal snd chn)</code>:<br>" +
                                  " #t if the time domain, fft, and lisp graphs are layed out horizontally";

var extsnd_graphstyle_tip = "<code>(graph-style snd chn)</code>: graph style, <br>" +
                            " one of <code>graph-lines graph-dots graph-dots-and-lines graph-lollipops graph-filled</code>";

var extsnd_headertype_tip = "<code>(header-type snd)</code>: snd's header type (e.g. <code>mus-aiff</code>)";

var extsnd_helpdialog_tip = "<code>(help-dialog subject message xrefs urls)</code>: start the Help window with subject and message";

var extsnd_hidewidget_tip = "<code>(hide-widget widget)</code>: hide or undisplay widget";

var extsnd_infodialog_tip = "<code>(info-dialog subject message)</code>: start the Info window with subject and message";

var extsnd_initialgraphhook_tip = "<code>initial-graph-hook (snd chn dur)</code>:<br>" +
                                  " called when a sound is displayed for the first time";

var extsnd_insertsound_tip = "<code>(insert-sound file (beg 0) (file-chan 0) snd chn edpos auto-delete)</code>:<br>" +
                             " insert channel file-chan of file (or all chans if file-chan is not given)<br>" +
                             " into snd's channel chn at beg or at the cursor position.<br>" +
                             "<code>  (insert-sound \"oboe.snd\" 1000)</code><br>" +
                             " inserts all of oboe.snd starting at sample 1000.";

var extsnd_justsounds_tip = "<code>(just-sounds)</code>: the 'just sounds' choice in the file chooser dialog";

var extsnd_keyboard_no_action_tip = "<code>keyboard-no-action</code> is one of the <code>bind-key</a> function<br>" +
                                    "return values.  It indicates that Snd should not update the graphs.";

var extsnd_leftsample_tip = "<code>(left-sample snd chn)</code>: left sample number in time domain window";

var extsnd_lispgraph_tip = "the <code>lisp-graph</code> is the 3rd graph displayed in the channel graphs.";

var extsnd_lispgraphhook_tip = "<code>lisp-graph-hook (snd chn)</code>: called just before the lisp graph is updated.<br>" +
                               " If it returns a list of pixels, these are used in order by the list of graphs<br>" +
                               " (if any), rather than Snd's default set; this makes it possible to use different<br>" +
                               " colors for the various graphs. If it returns a function (of no arguments),<br>" +
                               " that function is called rather than the standard graph routine.";

var extsnd_listenerfont_tip = "<code>(listener-font)</code>: font used by the lisp listener";

var extsnd_listenerprompt_tip = "<code>(listener-prompt)</code>: the current lisp listener prompt character ('&gt;') ";

var extsnd_listtovct_tip = "<code>(list-&gt;vct lst)</code>: returns a new vct filled with elements of list lst";

var extsnd_mainwidgets_tip = "<code>(main-widgets)</code>: top level widgets<br>" +
                             " <code>(list (0)main-app (1)main-shell (2)main-pane<br>" +
                             " (3)sound-pane (4)listener-pane (5)notebook-outer-pane)</code>";

var extsnd_makecolor_tip = "<code>(make-color r g b)</code>: return a color object with the indicated rgb values";

var extsnd_makegraphdata_tip = "<code>(make-graph-data snd chn edpos low high)</code>:<br>" +
                               " return either a vct (if the graph has one trace), or a list of two vcts<br>" +
                               " (the two sides of the envelope graph).<br>" +
                               " 'edpos' defaults to the current-edit-position,<br>" +
                               " 'low' defaults to the current window left sample, and<br>" +
                               " 'high' defaults to the current rightmost sample.<br>" +
                               " <code>(graph-data (make-graph-data))</code> reimplements the time domain graph.";

var extsnd_makemixsampler_tip = "<code>(make-mix-sampler id (beg 0))</code>:<br>" +
                                     " return a reader ready to access mix id";

var extsnd_makesampler_tip = "<code>(make-sampler (start-samp 0) snd chn (dir 1) edpos)</code>:<br>" +
                                  " return a reader ready to access snd's channel chn's data starting at start-samp,<br>" +
                                  " going in direction dir (1 = forward, -1 = backward),<br>" +
                                  " reading the version of the data indicated by edpos which defaults to the current version.<br>" +
                                  " snd can be a filename, or a sound index number.";

var extsnd_makesounddata_tip = "<code>(make-sound-data chans frames)</code>: return a new sound-data object<br>" +
                               " with 'chans' channels, each having 'frames' samples";

var extsnd_makevct_tip = "<code>(make-vct len (initial-element 0))</code>: <br>" +
                         " returns a new vct of length len filled with initial-element:<br>" +
                         "<code>  (define v (make-vct 32 1.0))</code>";

var extsnd_mapchannel_tip = "<code>(map-channel func (start 0) (dur len) snd chn edpos edname)</code>:<br>" +
                            " apply func to samples in current channel;<br>" +
                            " edname is the edit history name for this editing operation.<br>" +
                            "<code>  (map-channel (lambda (y) (* y 2.0)))</code>";

var extsnd_markclickhook_tip = "<code>mark-click-hook (id)</code>: called when a mark is clicked;<br>" +
                               " return #t to squelch the default message.";

var extsnd_markdraghook_tip = "<code>mark-drag-hook (id)</code>: called when a mark is dragged";

var extsnd_markhome_tip = "<code>(mark-home id)</code>: the sound (index) and channel that hold mark id";

var extsnd_markname_tip = "<code>(mark-name id)</code>: mark's name";

var extsnd_marksample_tip = "<code>(mark-sample id pos)</code>: mark's location (sample number) at edit history pos";

var extsnd_marksync_tip = "<code>(mark-sync id)</code>: mark's sync value (default: 0)";

var extsnd_marksyncmax_tip = "<code>(mark-sync-max)</code>: max mark sync value seen so far";

var extsnd_maxamp_tip = "<code>(maxamp snd chn edpos)</code>: maxamp of data in snd's channel chn";

var extsnd_mix_tip = "<code>(mix file (beg 0) (file-chan 0) snd chn (with-tag with-mix-tags) auto-delete)</code>:<br>" +
                     " mix channel file-chan of file into snd's channel chn starting at beg (in the output),<br>" +
                     " returning the new mix's id.  if with-tag is #f, no draggable tag is created. <br>" +
                     " If auto-delete is #t, the input file is deleted when it is no longer needed.";

var extsnd_mixamp_tip = "<code>(mix-amp id)</code>: mix's scaler";

var extsnd_mixcolor_tip = "<code>(mix-color mix-id)</code>: color of all mix tags<br>" +
                          " (if mix-id is omitted), or of mix-id's tag";

var extsnd_mixposition_tip = "<code>(mix-position id)</code>: mix's begin time in the output in samples";

var extsnd_mixreleasehook_tip = "<code>mix-release-hook (mix-id samps)</code>:<br>" +
                                " called after the mouse has dragged a mix to some new position.<br>" +
                                " 'samps' = samples moved in the course of the drag.<br>" +
                                " If the hook returns #t, the actual remix is the hook's responsibility.";

var extsnd_mixsamplerQ_tip = "<code>(mix-sampler? obj)</code>: #t if obj is a mix-sampler";

var extsnd_mixselection_tip = "<code>(mix-selection (beg 0) snd chn (selection-channel #t))</code>:<br>" +
                              " mix the currently selected portion starting at beg";

var extsnd_mixsync_tip = "<code>(mix-sync id)</code>: mix sync field (an integer)";

var extsnd_mixsyncmax_tip = "<code>(mix-sync-max)</code>: max mix sync value seen so far";

var extsnd_mixtagy_tip = "<code>(mix-tag-y id)</code>: height of mix's tag";

var extsnd_musaudioclose_tip = "<code>(mus-audio-close line)</code>: close the audio hardware line";

var extsnd_musaudioopenoutput_tip = "<code>(mus-audio-open-output device srate chans format bytes)</code>:<br>" +
                                    " open the audio device ready for output at the given srate and so on;<br>" +
                                    " return the audio line number:<br>" +
                                    "<code>  (mus-audio-open-output mus-audio-default 22050 1 mus-lshort 256)</code>";

var extsnd_musaudiowrite_tip = "<code>(mus-audio-write line sdata frames)</code>:<br>" +
                               " write frames of data (channels * frames = samples) to the audio line from sound-data sdata.";

var extsnd_musbfloat_tip = "<code>mus-bfloat</code> data is big-endian float";

var extsnd_musbshort_tip = "<code>mus-bshort</code> data is big-endian signed 16-bit integer";

var extsnd_musdataformatname_tip = "<code>(mus-data-format-name format)</code>: data format (e.g. mus-bshort) as a string";

var extsnd_musheadertypename_tip = "<code>(mus-header-type-name type)</code>: header type (e.g. mus-aiff) as a string";

var extsnd_muslfloat_tip = "<code>mus-lfloat</code> data is little-endian float";

var extsnd_muslshort_tip = "<code>mus-lshort</code> data is little-endian signed 16-bit integer";

var extsnd_musosssetbuffers_tip = "<code>(mus-oss-set-buffers num size)</code>: set Linux OSS 'fragment' number and size.<br>" +
                                  " If Snd's controls seem sluggish, try <code>(mus-oss-set-buffers 4 12)</code><br>" +
                                  " or even <code>(mus-oss-set-buffers 2 12)</code>.<br>" +
                                  " This reduces the on-card buffering, but may introduce clicks.";

var extsnd_mussoundchans_tip = "<code>(mus-sound-chans filename)</code>: channels of data in sound file";

var extsnd_mussoundcloseinput_tip = "<code>(mus-sound-close-input fd)</code>: close (low-level) file fd that was opened by mus-sound-open-input.";

var extsnd_mussoundcomment_tip = "<code>(mus-sound-comment filename)</code>: comment (a string) found in sound file's header";

var extsnd_mussounddataformat_tip = "<code>(mus-sound-data-format filename)</code>: data format (e.g. mus-bshort) of data in sound file";

var extsnd_mussoundduration_tip = "<code>(mus-sound-duration filename)</code>: duration (in seconds) of sound file";

var extsnd_mussoundframes_tip = "<code>(mus-sound-frames filename)</code>: frames (samples / channel) in sound file";

var extsnd_mussoundheadertype_tip = "<code>(mus-sound-header-type filename)</code>: header type (e.g. mus-aifc) of sound file";

var extsnd_mussoundloopinfo_tip = "<code>(mus-sound-loop-info filename)</code>: synth loop info for sound as a list:<br>" +
                                  "<code> (start1 end1 start2 end2 base-note base-detune mode1 mode2)</code>";

var extsnd_mussoundmaxamp_tip = "<code>(mus-sound-maxamp filename)</code>: maxamps in sound<br>" +
                                " (a list of paired amps (as floats) and locations (in samples))";

var extsnd_mussoundmaxampexists_tip = "<code>(mus-sound-maxamp-exists? filename)</code>: #t if sound's maxamp data is available;<br>" +
                                      " if it isn't, a call on mus-sound-maxamp has to open and read the data to get the maxamp.";

var extsnd_mussoundopeninput_tip = "<code>(mus-sound-open-input filename)</code>: open filename for (low-level) sound input,<br>" +
                                   " return file descriptor (an integer)";

var extsnd_mussoundread_tip = "<code>(mus-sound-read fd beg end chans sdata)</code>: read sound data from file fd,<br>" +
                              " filling sound-data sdata's buffers starting at beg (buffer location), going to end";

var extsnd_mussoundsamples_tip = "<code>(mus-sound-samples filename)</code>: samples (frames * channels) in sound file";

var extsnd_mussoundsrate_tip = "<code>(mus-sound-srate filename)</code>: sampling rate of sound file";

var extsnd_nameclickhook_tip = "<code>name-click-hook (snd)</code>: called when sound name clicked.<br>" +
                               " If it returns #t, the usual informative status area babbling is squelched.";

var extsnd_newsound_tip = "<code>(new-sound :file :header-type :data-format :srate :channels :comment :size)</code>:<br>" +
                          " creates a new sound file with the indicated attributes; if any are omitted,<br>" +
                          " the corresponding default-output variable is used. <br>" +
                          " The 'size' argument sets the number of samples (zeros) in the newly created sound.<br>" +
                          "<code>  (new-sound \"test.snd\" mus-next mus-bshort 22050 1 \"no comment\" 1000)</code>";

var extsnd_nextsample_tip = "<code>(next-sample reader)</code>: next sample from reader";

var extsnd_normalizefft_tip = "<code>(transform-normalization snd chn)</code>:<br>" +
                              " decides whether spectral data is normalized before display;<br>" +
                              " can be dont-normalize, normalize-by-channel (default), normalize-by-sound, or normalize-globally.";

var extsnd_openfiledialog_tip = "<code>(open-file-dialog (managed #t))</code>:<br>" +
                                " create the file dialog if needed and display it if 'managed'";

var extsnd_openhook_tip = "<code>open-hook (filename)</code>: called each time a file is opened<br>" +
                          " (before the actual open). If it returns #t, the file is not opened.";

var extsnd_openrawsoundhook_tip = "<code>open-raw-sound-hook (filename current-choices)</code>:<br>" +
                                  " called when a headerless sound file is opened.<br>" +
                                  " Its result can be a list describing the raw file's attributes <br>" +
                                  " (thereby bypassing the Raw File Dialog and so on).<br>" +
                                  " The list (passed to subsequent hook functions as 'current-choice')<br>" +
                                  " is interpreted as <code>(list chans srate data-format data-location data-length)</code><br>" +
                                  " where trailing elements can be omitted (location defaults to 0,<br>" +
                                  " and length defaults to the file length in bytes).";

var extsnd_opensound_tip = "<code>(open-sound filename)</code>: open filename <br>" +
                           " (as if opened from File:Open menu option), and return the new sound's index";

var extsnd_padchannel_tip = "<code>(pad-channel beg dur snd chn edpos)</code>: insert dur zeros at beg in snd's chn";

var extsnd_peaksfont_tip = "<code>(peaks-font)</code>: normal font used by fft peak display";

var extsnd_play_tip = "<code>(play object :start :end :channel :edit-position :out-channel :with-sync :wait :stop):<br>" +
                      " play the object from start to end.<br>" +
                      " If channel is not given, play all channels.<br>" +
                      " If with-sync, play all objects sync'd to the current object.<br>" +
                      " If wait, wait for the play process to finish before going on.<br>" +
                      " If out-channel, send the samples to that DAC channel.<br>" +
                      " If edit-position, play that member of the edit list.<br>" +
                      " If stop, call that function when the play process finishes.<br>" +
                      " If object is a string, it is assumed to be a file name.";

var extsnd_playhook_tip = "<code>play-hook (samps)</code>: called each time a buffer is sent to the DAC.";

var extsnd_positiontox_tip = "<code>(position-&gt;x val snd chn (ax time-graph))</code>: x axis value corresponding to pixel val";

var extsnd_previoussample_tip = "<code>(previous-sample reader)</code>: previous sample from reader";

var extsnd_readmixsample_tip = "<code>(read-mix-sample reader)</code>: read sample from mix reader";

var extsnd_readonly_tip = "<code>(read-only snd)</code>: whether snd is write-protected";

var extsnd_readsample_tip = "<code>(read-sample reader)</code>: get the next sample from the sampler";

var extsnd_regionframes_tip = "<code>(region-frames (reg 0) (chan 0))</code>: region length in frames";

var extsnd_regionok_tip = "<code>(region? reg)</code>: #t if region is active";

var extsnd_regularizedargs_tip = "The \"regularized\" functions take arguments in the order<br>" +
                                 " begin time, duration (not end sample), sound index, channel number, and edit position.";

var extsnd_statusreport_tip = "<code>(status-report msg snd)</code>: display msg in snd's status area.";

var extsnd_restorecontrols_tip = "<code>(restore-controls snd)</code>: restore the previously saved control panel settings";

var extsnd_reversesound_tip = "<code>(reverse-sound snd chn edpos)</code>: reverse snd's channel chn";

var extsnd_revertsound_tip = "<code>(revert-sound snd)</code>: return 'snd' to its unedited state (undo all edits).";

var extsnd_rightsample_tip = "<code>(right-sample snd chn)</code>: right sample number in time domain window";

var extsnd_sample_tip = "<code>(sample samp snd chn edpos)</code>:<br>" +
                        " return sample samp in snd's channel chn<br>" +
                        " (this is a slow access -- use samplers for speed)";

var extsnd_sampleratendQ_tip = "<code>(sampler-at-end? obj)</code>: #t if sampler has reached the end of its data";

var extsnd_samples_tip = "<code>(samples (start-samp 0) (samps len) snd chn edpos)</code>:<br>" +
                         " return a vct containing snd channel chn's samples starting a start-samp for samps samples;<br>" +
                         " edpos is the edit history position to read (defaults to current position).";

var extsnd_savedir_tip = "<code>(save-dir)</code>: name of directory for saved state data (or #f=null)";

var extsnd_savehook_tip = "<code>save-hook (snd name)</code>: called each time a file is about to be saved.<br>" +
                          " If it returns #t, the file is not saved.<br>" +
                          " 'name' is #f unless the file is being saved under a new name (as in sound-save-as).";

var extsnd_savesound_tip = "<code>(save-sound snd)</code>: save snd<br>" +
                           " (update the on-disk data to match Snd's current version)";

var extsnd_savesoundas_tip = "<code>(save-sound-as :file :sound :header-type :data-format :srate :channel :edit-position :comment)</code>:<br>" +
                             " save sound in file using the indicated attributes.<br>" +
                             " If channel is specified, only that channel is saved (extracted).<br>" +
                             " Omitted arguments take their value from the sound being saved.<br>" +
                             "<code>   (save-sound-as \"test.snd\" index mus-next mus-bshort)</code>";

var extsnd_savestatehook_tip = "<code>save-state-hook (temp-filename)</code>: called each time the save-state<br>" +
                               " mechanism is about to create a new temporary file to save some edit history<br>" +
                               " sample values. temp-filename is the current file.<br>" +
                               " If the hook returns a string, it is treated as the new temp filename.<br>" +
                               " This hook provides a way to keep track of which files are in a given<br>" +
                               " saved state batch, and a way to rename or redirect those files.";

var extsnd_scaleby_tip = "<code>(scale-by scalers snd chn)</code>: scale snd by scalers (following sync);<br>" +
                         " scalers can be a float or a vct/list of floats";

var extsnd_scalechannel_tip = "<code>(scale-channel scaler (beg 0) (dur len) snd chn edpos)</code>:<br>" +
                              " scale samples in the given sound/channel between beg and beg + num by scaler.";

var extsnd_scaleselectionby_tip = "<code>(scale-selection-by scalers)</code>: scale selected portion by scalers";

var extsnd_scaleto_tip = "<code>(scale-to (norms 1.0) snd chn)</code>: normalize snd to norms (following sync);<br>" +
                         " norms can be a float or a vct/list of floats";

var extsnd_scanchannel_tip = "<code>(scan-channel func (start 0) (dur len) snd chn edpos)</code>:<br>" +
                             " apply func to samples in current channel (or the specified channel).<br>" +
                             " func is a function of one argument, the current sample.<br>" +
                             " if func returns non-#f, the scan stops, and the value is returned to the caller<br>" +
                             " with the sample number.<br>" +
                             "<code>   (scan-channel (lambda (y) (> y .1)))</code>";

var extsnd_scriptarg_tip = "<code>(script-arg)</code>: where we are in the startup arg list";

var extsnd_scriptargs_tip = "<code>(script-args)</code>: the args passed to Snd at startup as a list of strings";

var extsnd_searchprocedure_tip = "<code>(search-procedure snd)</code>: global search function<br>" +
                                 " (if no 'snd' specified) or sound-local search function";

var extsnd_selectall_tip = "<code>(select-all snd chn)</code>: make a new selection containing all of snd's channel chn.<br>" +
                           " If sync is set, all chans are included. <br>" +
                           " The new region id is returned (if selection-creates-region is #t).";

var extsnd_selectedchannel_tip = "<code>(selected-channel snd)</code>: currently selected channel in snd (or #f if none)";

var extsnd_selecteddatacolor_tip = "<code>(selected-data-color)</code>: color used for selected data";

var extsnd_selectedgraphcolor_tip = "<code>(selected-graph-color)</code>: background color of selected data";

var extsnd_selectedsound_tip = "<code>(selected-sound)</code>: index of currently selected sound (or #f if none)";

var extsnd_selectioncreatesregion_tip = "<code>(selection-creates-region)</code>: #t if a region should be created each time a selection is made.<br>" +
                                        " The default is currently #t, but that may change.<br>" +
                                        " If you're dealing with large selections, and have no need of regions (saved selections),<br>" +
                                        " you can speed up many operations by setting this flag to #f";

var extsnd_selectionframes_tip = "<code>(selection-frames snd chn)</code>: selection length";

var extsnd_selectionmember_tip = "<code>(selection-member? snd chn)</code>: #t if snd's channel chn is a member of the current selection";

var extsnd_selectionok_tip = "<code>(selection?)</code>: #t if selection is currently active, visible, etc";

var extsnd_selectionposition_tip = "<code>(selection-position snd chn)</code>: selection start samp";

var extsnd_setsamples_tip = "<code>(set-samples start-samp samps data snd chn truncate edname (infile-chan 0) edpos auto-delete)</code>:<br>" +
                            " set snd's channel chn's samples starting at start-samp for samps from data (a vct, vector, or string (filename));<br>" +
                            " start-samp can be beyond current data end;<br>" +
                            " if truncate is #t and start-samp is 0, the end of the file is set to match the new data's end.";

var extsnd_shortfilename_tip = "<code>(short-file-name snd)</code>: short form of snd's file name (no directory)";

var extsnd_showlistener_tip = "<code>(show-listener (open #t))</code>: if 'open' opens the lisp listener;<br>" +
                              " returns whether the listener is visible.";

var extsnd_showtransformpeaks_tip = "<code>(show-transform-peaks snd chn)</code>: #t if fft display should include peak list";

var extsnd_sndhelp_tip = "<code>(snd-help (arg 'snd-help) (formatted #t))</code>: return the documentation associated with its argument.<br>" +
                         "<code> (snd-help 'make-vct)</code> for example, prints out a brief description of make-vct.<br>" +
                         " The argument can be a string, symbol, or in some cases, the object itself.<br>" +
                         " In the help descriptions, optional arguments are in parens with the default value (if any) as the 2nd entry.<br>" +
                         " A ':' as the start of the argument name marks a CLM-style optional keyword argument. <br>" +
                         " If you load index.scm the functions html and ? can be used in place of help to go to the HTML description,<br>" +
                         " and the location of the associated C code will be displayed, if it can be found.<br>" +
                         " If help-hook is not empty, it is invoked with the subject and the snd-help result and its value is returned.";

var extsnd_sndprint_tip = "<code>(snd-print str)</code>: display str in the listener window";

var extsnd_sndspectrum_tip = "<code>(snd-spectrum data (window rectangular-window) (len data-len)<br>" +
                             " (linear #t) (beta 0.0) in-place (normalized #t))</code>:<br>" +
                             " magnitude spectrum of data (a vct), in data if in-place, using fft-window win and fft length len.";

var extsnd_sndtempnam_tip = "<code>(snd-tempnam)</code>: return a new temp file name using temp-dir.";

var extsnd_sounddataref_tip = "<code>(sound-data-ref sd chan i)</code>: sample in channel chan at location i of sound-data sd:<br>" +
                              " sd[chan][i]";

var extsnd_sounddataset_tip = "<code>(sound-data-set! sd chan i val)</code>: set sound-data sd's i-th element in channel chan to val:<br>" +
                              " sd[chan][i] = val";

var extsnd_sounddata_times_tip = "<code>(sound-data* val1 val2)</code>: multiply val1 by val2 (either or both can be a sound-data object).";

var extsnd_soundfilep_tip = "<code>(sound-file? name)</code>: #t if name has a known sound file extension";

var extsnd_soundfilesindirectory_tip = "<code>(sound-files-in-directory (directory \".\"))</code>:<br>" +
                                       " return a list of the sound files in 'directory'";

var extsnd_soundp_tip = "<code>(sound? (index 0))</code>: #t if sound associated with 'index' is active (accessible)";

var extsnd_sounds_tip = "<code>(sounds)</code>: list of active sounds (a list of indices)";

var extsnd_soundwidgets_tip = "<code>(sound-widgets snd)</code>: returns a list of widgets associated with 'snd':<br>" +
                              "(0)pane (1)name (2)control-panel<br>" +
                              "(3)status area (4)play-button (5)filter-env<br>" +
                              "(6)unite-button (7)name-label (8)name-icon (9)sync-button";

var extsnd_squelchupdate_tip = "<code>(squelch-update snd chn)</code>: #t if updates (redisplays) are turned off in snd's channel chn";

var extsnd_srate_tip = "<code>(srate snd)</code>: snd's srate";

var extsnd_srcchannel_tip = "<code>(src-channel ratio-or-env (beg 0) (dur len) snd chn edpos)</code>:<br>" +
                            " sampling-rate convert snd's channel chn by ratio, or following an envelope <br>" +
                            " (a list or a CLM env generator).";

var extsnd_srcsound_tip = "<code>(src-sound ratio-or-env (base 1.0) snd chn edpos)</code>:<br>" +
                          " sampling-rate convert snd's channel chn by ratio, or following an envelope.<br>" +
                          " A negative ratio reverses the sound";

var extsnd_selectionmaxamp_tip = "<code>(selection-maxamp)</code> returns the peak amplitude in the selection.";

var extsnd_startplayinghook_tip = "<code>start-playing-hook (snd)</code>: called when a play request is triggered.<br>" +
                                  " If it returns #t, the sound is not played.";

var extsnd_startplayingselectionhook_tip = "<code>start-playing-selection-hook ()</code>: called when the selection starts playing";

var extsnd_stopdachook_tip = "<code>stop-dac-hook ()</code>: called upon mus_audio_close (when DAC is turned off)";

var extsnd_stopplaying_tip = "<code>(stop-playing snd)</code>: stop play (DAC output) in progress";

var extsnd_stopplayinghook_tip = "<code>stop-playing-hook (snd)</code>: called when a sound finishes playing.";

var extsnd_stopplayingselectionhook_tip = "<code>stop-playing-selection-hook ()</code>: called when the selection stops playing";

var extsnd_sync_tip = "<code>(sync snd)</code>: snd's sync value (0 = no sync).<br>" +
                      "  Some editing operations are applied to all sounds sharing the sync value of the selected sound.";

var extsnd_tempdir_tip = "<code>(temp-dir)</code>: name of directory for temp files (or #f=null)";

var extsnd_time_graph_tip = "<code>time-graph<code> is the constant associated with the time domain graph<br>" +
                            "The other two graphs are <code>transform-graph</code> and <code>lisp-graph</code>";

var extsnd_timegraphtype_tip = "<code>(time-graph-type snd chn)</code>: graph-as-wavogram if<br>" +
                               " Snd's time domain display is a 'wavogram',otherwise graph-once.";

var extsnd_tinyfont_tip = "<code>(tiny-font)</code>: font use for some info in the graphs";

var extsnd_transformgraphp_tip = "<code>(transform-graph? snd chn)</code>: #t if fft display is active in snd's channel chn";

var extsnd_transformgraphtype_tip = "<code>(transform-graph-type snd chn)</code>: can be<br>" +
                                    " graph-once, graph-as-sonogram, or graph-as-spectrogram.";

var extsnd_transformsize_tip = "<code>(transform-size snd chn)</code>: current fft size (512)";

var extsnd_transformtovct_tip = "<code>(transform-&gt;vct snd chn obj)</code>: return a vct (obj if it's passed),<br>" +
                                " with the current transform data from snd's channel chn";

var extsnd_undo_tip = "<code>(undo (count 1) snd chn)</code>: undo 'count' edits in snd's channel chn";

var extsnd_updatesound_tip = "<code>(update-sound snd)</code>: update snd (re-read it from the disk after flushing pending edits)";

var extsnd_updatetimegraph_tip = "<code>(update-time-graph snd chn)</code>: redraw snd channel chn's graphs";

var extsnd_updatetransformgraph_tip = "<code>(update-transform-graph snd chn)</code>: redraw snd channel chn's fft display";

var extsnd_vct_tip = "<code>(vct :rest args)</code>: returns a new vct with args as contents; same as list-&gt;vct: (vct 1 2 3)";

var extsnd_vctadd_tip = "<code>(vct-add! v1 v2 (offset 0))</code>: element-wise add of vcts v1 and v2: v1[i + offset] += v2[i], returns v1";

var extsnd_vctcopy_tip = "<code>(vct-copy v)</code>: returns a copy of vct v";

var extsnd_vctfill_tip = "<code>(vct-fill! v val)</code>: set each element of v to val: v[i] = val, returns v";

var extsnd_vctmove_tip = "<code>(vct-move! obj new old backwards)</code>: moves vct obj data from old to new:<br>" +
                         " v[new++] = v[old++], or v[new--] = v[old--] if backwards is #f.";

var extsnd_vctmultiply_tip = "<code>(vct-multiply! v1 v2)</code>: element-wise multiply of vcts v1 and v2:<br>" +
                             " v1[i] *= v2[i], returns v1";

var extsnd_vctoffset_tip = "<code>(vct-offset! v val)</code>: add val to each element of v:<br>" +
                           " v[i] += val, returns v";

var extsnd_vctp_tip = "<code>(vct? obj)</code>: is obj a vct";

var extsnd_vctpeak_tip = "<code>(vct-peak v)</code>: max of abs of elements of v";

var extsnd_vctref_tip = "<code>(vct-ref v n)</code>: element n of vct v, v[n]";

var extsnd_vctreverse_tip = "<code>(vct-reverse! vct len)</code>: in-place reversal of vct contents";

var extsnd_vctscale_tip = "<code>(vct-scale! v val)</code>: scale each element of v by val:<br>" +
                          " v[i] *= val, returns v";

var extsnd_vctset_tip = "<code>(vct-set! v n val)</code>: sets element of vct v to val, v[n] = val";

var extsnd_vctsubseq_tip = "<code>(vct-subseq v start end vnew)</code>: v[start..end],<br>" +
                           " placed in vnew if given or new vct";

var extsnd_vcttochannel_tip = "<code>(vct-&gt;channel vct (beg 0) (dur len) snd chn edpos origin)</code>:<br>" +
                              " set snd's channel chn's samples starting at beg for dur samps from vct data";

var extsnd_vcttosounddata_tip = "<code>(vct-&gt;sound-data v sd chan)</code>: copies vct v's data into sound-data sd's channel chan";

var extsnd_widgetposition_tip = "<code>(widget-position wid)</code>: widget's position, (list x y), in pixels";

var extsnd_widgetsize_tip = "<code>(widget-size wid)</code>: widget's size, (list width height), in pixels";

var extsnd_windowheight_tip = "<code>(window-height)</code>: current Snd window height in pixels";

var extsnd_windowwidth_tip = "<code>(window-width)</code>: current Snd window width in pixels";

var extsnd_withmixtags_tip = "<code>(with-mix-tags)</code>: #t if Snd should try to use virtual (tagged) mixing";

var extsnd_withtrackingcursor_tip = "<code>(with-tracking-cursor snd)</code>:<br>#t if cursor moves along in waveform display as sound is played";

var extsnd_xaxislabel_tip = "<code>(x-axis-label snd chn (ax time-graph))</code>: current x axis label";

var extsnd_xaxisstyle_tip = "<code>(x-axis-style snd chn)</code>: The x axis labelling of the time domain waveform<br>" +
                            " can be in seconds (x-axis-in-seconds), in samples (x-axis-in-samples),<br>" +
                            " expressed as a percentage of the overall duration (x-axis-as-percentage),<br>" +
                            " as a beat number (x-axis-in-beats), as a measure number (x-axis-in-measures),<br>" +
                            " or clock-style (dd:hh:mm:ss) (x-axis-as-clock).";

var extsnd_xbounds_tip = "<code>(x-bounds snd chn)</code>:<br>a list (x0 x1) giving the current x axis bounds of snd channel chn";

var extsnd_xtoposition_tip = "<code>(x-&gt;position val snd chn (ax time-graph))</code>: x pixel loc of val";

var extsnd_xzoomslider_tip = "<code>(x-zoom-slider snd chn)</code>: current x axis zoom slider of snd channel chn";

var extsnd_ybounds_tip = "<code>(y-bounds snd chn)</code>:<br>a list (low high) giving the current y axis bounds of snd channel chn";

var extsnd_ytoposition_tip = "<code>(y-&gt;position val snd chn (ax time-graph))</code>: y pixel loc of val";

var extsnd_yzoomslider_tip = "<code>(y-zoom-slider snd chn)</code>: current y axis zoom slider of snd channel chn";

var sndclm_amplitude_modulate_tip = "<code>(amplitude-modulate carrier in1 in2)</code>: in1 * (carrier + in2)";

var sndclm_array_interp_tip = "<code>(array-interp v phase size)</code>: v[phase] taking into account wrap-around<br>" +
                              " (size is size of data), with linear interpolation if phase is not an integer.";

var sndclm_comb_tip = "<code>(comb gen (val 0.0) (pm 0.0))</code>: comb filter val, pm changes the delay length.";

var sndclm_contrast_enhancement_tip = "<code>(contrast-enhancement sig (index 1.0))</code>: sin(sig * pi / 2 + index * sin(sig * 2 * pi))";

var sndclm_convolve_tip = "<code>(convolve gen input-func)</code>: next sample from convolution generator";

var sndclm_delay_tip = "<code>(delay gen (val 0.0) (pm 0.0))</code>: delay val<br>" +
                       " according to the delay line's length and pm ('phase-modulation').<br>" +
                       " If pm is greater than 0.0, the max-size argument used to create gen<br>" +
                       " should have accommodated its maximum value.";

var sndclm_dot_product_tip = "<code>(dot-product v1 v2 size)</code>: sum of (vcts) v1[i] * v2[i] (also named scalar product)";

var sndclm_env_tip = "<code>(env gen)</code>: next sample from envelope generator";

var sndclm_fft_tip = "<code>(mus-fft rl im len (dir 1))</code>:<br>" +
                     " return the fft of vcts rl and im which contain <br>" +
                     " the real and imaginary parts of the data;<br>" +
                     " len should be a power of 2,<br>" +
                     " dir = 1 for fft, -1 for inverse-fft";

var sndclm_filetoarray_tip = "<code>(file-&gt;array filename chan start samples data)</code>:<br>" +
                             " read the sound file 'filename' placing samples from channel 'chan'<br>" +
                             " into the vct 'data' starting in the file at frame 'start'<br>" +
                             " and reading 'samples' samples altogether.";

var sndclm_filetosample_tip = "<code>(file-&gt;sample obj frame chan)</code>: sample value in sound file read by 'obj' in channel chan at frame";

var sndclm_fir_filter_tip = "<code>(fir-filter gen (input 0.0))</code>: next sample from FIR filter";

var sndclm_formant_tip = "<code>(formant gen (input 0.0) freq-in-radians)</code>: next sample from resonator generator";

var sndclm_frame_times_tip = "<code>(frame* f1 f2 outf)</code>: multiply f1 and f2 (elementwise)<br>" +
                             " returning outf; if outf is not given, a new frame is created.<br>" +
                             " outf[i] = f1[i] * f2[i].";

var sndclm_granulate_tip = "<code>(granulate gen input-func edit-func)</code>: next sample from granular synthesis generator";

var sndclm_hztoradians_tip = "<code>(hz-&gt;radians hz)</code>: convert frequency in Hz to radians per sample: hz * 2 * pi / srate";

var sndclm_in_any_tip = "<code>(in-any frame chan stream)</code>: input stream sample at frame in channel chan";

var sndclm_ina_tip = "<code>(ina frame stream)</code>: input stream sample in channel 0 at frame";

var sndclm_locsig_tip = "<code>(locsig gen loc val)</code>: add 'val' to the output of locsig at frame 'loc'";

var sndclm_make_comb_tip = "<code>(make-comb :scaler :size :initial-contents (:initial-element 0.0) :max-size (:type mus-interp-linear))</code>:<br>" +
                           " return a new comb filter (a delay line with a scaler on the feedback) of size elements.<br>" +
                           " If the comb length will be changing at run-time, max-size sets its maximum length.<br>" +
                           " initial-contents can be either a list or a vct.";

var sndclm_contrast_enhancement_tip = "<code>(contrast-enhancement input (fm-index 1.0))</code><br>" +
                           " phase-modulates its input.";

var sndclm_make_convolve_tip = "<code>(make-convolve :input :filter :fft-size)</code>: <br>" +
                               " return a new convolution generator which convolves its input with the impulse response 'filter'.";

var sndclm_make_delay_tip = "<code>(make-delay :size :initial-contents (:initial-element 0.0) (:max-size) (:type mus-interp-linear))</code>:<br>" +
                            " return a new delay line of size elements.<br>" +
                            " If the delay length will be changing at run-time, max-size sets its maximum length,<br>" +
                            " so <code>(make-delay len :max-size (+ len 10))</code> provides 10 extra elements of delay<br>" +
                            " for subsequent phasing or flanging.<br>" +
                            " initial-contents can be either a list or a vct.";

var sndclm_make_env_tip = "<code>(make-env :envelope (:scaler 1.0) :duration (:offset 0.0) (:base 1.0) :end :length)</code>:<br>" +
                          " return a new envelope generator.<br>" +
                          " 'envelope' is a list or vct of break-point pairs. To create the envelope,<br>" +
                          " these points are offset by 'offset', scaled by 'scaler', and mapped over the time interval<br>" +
                          " defined by either 'duration' (seconds) or 'length' (samples).<br>" +
                          " If 'base' is 1.0, the connecting segments are linear, if 0.0 you get a step function,<br>" +
                          " and anything else produces an exponential connecting segment.";

var sndclm_make_filetosample_tip = "<code>(make-file-&gt;sample filename buffer-size)</code>:<br>" +
                                   " return an input generator reading 'filename' (a sound file)";

var sndclm_make_filter_tip = "<code>(make-filter :order :xcoeffs :ycoeffs)</code>:<br>" +
                             " return a new direct form FIR/IIR filter, coeff args are vcts";

var sndclm_make_fir_filter_tip = "<code>(make-fir-filter :order :xcoeffs)</code>: return a new FIR filter, xcoeffs a vct";

var sndclm_make_formant_tip = "<code>(make-formant :frequency :radius)</code>:<br>" +
                              " return a new formant generator (a resonator).<br>" +
                              " radius sets the pole radius (in terms of the 'unit circle').<br>" +
                              " frequency sets the resonance center frequency (Hz).";

var sndclm_make_frame_tip = "<code>(make-frame chans val0 val1 ...)</code>:<br>" +
                            " return a new frame object with chans samples,<br>" +
                            " each sample set from the trailing arguments (defaulting to 0.0):<br>" +
                            "<code>  (make-frame 2 .1 .2)</code>";

var sndclm_make_granulate_tip = "<code>(make-granulate :input (:expansion 1.0) (:length .15) (:scaler .6) (:hop .05)<br>" +
                                "       (:ramp .4) (:jitter 1.0) :max-size :edit)</code>:<br>" +
                                " return a new granular synthesis generator.<br>" +
                                " 'length' is the grain length (seconds),<br>" +
                                " 'expansion' is the ratio in timing between the new and old (expansion > 1.0 slows things down),<br>" +
                                " 'scaler' scales the grains to avoid overflows,<br>" +
                                " 'hop' is the spacing (seconds) between successive grains upon output,<br>" +
                                " 'jitter' controls the randomness in that spacing,<br>" +
                                " 'input' can be a file pointer.<br>" +
                                " 'edit' can be a function of one arg, the current granulate generator.<br>" +
                                "  It is called just before a grain is added into the output buffer.<br>" +
                                " The current grain is accessible via mus-data.<br>" +
                                " The edit function, if any, should return the length in samples of the grain, or 0.";

var sndclm_make_locsig_tip = "<code>(make-locsig (:degree 0.0) (:distance 1.0) (:reverb 0.0) (:output *output*) (:revout *reverb*)<br>" +
                             " (:channels (mus-channels *output*)) (:type mus-interp-linear))</code>:<br>" +
                             " return a new generator for signal placement in n channels.  Channel 0 corresponds to 0 degrees.";

var sndclm_make_moving_average_tip = "<code>(make-moving-average :size :initial-contents (:initial-element 0.0))</code>:<br>" +
                                     " return a new moving_average generator. initial-contents can be either a list or a vct.";

var sndclm_moving_max_tip = "<code>(moving-max gen y)</code>: return moving window max given input 'y'.<br>" +
                           " moving-max is a specialization of the delay generator that produces<br>" +
                           " an envelope that tracks the peak amplitude of the last 'size' samples.";

var sndclm_make_ncos_tip = "<code>(make-ncos (:frequency *clm-default-frequency*) (:n 1))</code>:<br>" +
                           " return a new ncos generator, producing a sum of 'n' equal amplitude cosines.";

var sndclm_make_one_pole_tip = "<code>(make-one-pole :a0 :b1)</code>: return a new one-pole filter; a0*x(n) - b1*y(n-1)";

var sndclm_make_one_zero_tip = "<code>(make-one-zero :a0 :a1)</code>: return a new one-zero filter;  a0*x(n) + a1*x(n-1)";

var sndclm_make_oscil_tip = "<code>(make-oscil (:frequency *clm-default-frequency*) (:initial-phase 0.0))</code>:<br>" +
                            " return a new oscil (sinewave) generator";

var sndclm_make_phase_vocoder_tip = "<code>(make-phase-vocoder :input :fft-size :overlap :interp :pitch :analyze :edit :synthesize)</code>:<br>" +
                                    " return a new phase-vocoder generator;<br>" +
                                    " input is the input function (it can be set at run-time),<br>" +
                                    " analyze, edit, and synthesize are either #f or functions that replace the default innards of the generator,<br>" +
                                    " fft-size, overlap and interp set the fftsize, the amount of overlap between ffts, and the time between new analysis calls.<br>" +
                                    " 'analyze', if given, takes 2 args, the generator and the input function;<br>" +
                                    " if it returns #t, the default analysis code is also called.<br>" +
                                    "  'edit', if given, takes 1 arg, the generator; if it returns #t, the default edit code is run.<br>" +
                                    "  'synthesize' is a function of 1 arg, the generator; it is called to get the current vocoder output.";

var sndclm_make_polyshape_tip = "<code>(make-polyshape (:frequency *clm-default-frequency*) (:initial-phase 0.0) :coeffs (:partials '(1 1)) (:kind mus-chebyshev-first-kind))</code>:<br>" +
                                " return a new polynomial-based waveshaping generator:<br>" +
                                "<code>   (make-polyshape :coeffs (partials-&gt;polynomial '(1 1.0)))</code><br>" +
                                " is the same in effect as make-oscil";

var sndclm_make_polywave_tip = "<code>(make-polyshape (:frequency *clm-default-frequency*) (:partials '(1 1)) (:kind mus-chebyshev-first-kind))</code>:<br>" +
                               " return a new polynomial-based waveshaping generator (additive synthesis).";

var sndclm_make_pulse_train_tip = "<code>(make-pulse-train (:frequency *clm-default-frequency*) (:amplitude 1.0) (:initial-phase 0.0))</code>:<br>" +
                                  " return a new pulse-train generator.  This produces a sequence of impulses.";

var sndclm_make_rand_interp_tip = "<code>(make-rand-interp (:frequency *clm-default-frequency*) (:amplitude 1.0) :envelope :distribution :size)</code>:<br>" +
                                  " return a new rand-interp generator, producing linearly interpolated random numbers.<br>" +
                                  " frequency is the rate at which new end-points are chosen.";

var sndclm_make_rand_tip = "<code>(make-rand (:frequency *clm-default-frequency*) (:amplitude 1.0) :envelope :distribution :size)</code>:<br>" +
                           " return a new rand generator, producing a sequence of random numbers (a step  function).<br>" +
                           " frequency is the rate at which new numbers are chosen.";

var sndclm_make_readin_tip = "<code>(make-readin :file (:channel 0) (:start 0) (:direction 1) :size)</code>:<br>" +
                             " return a new readin (file input) generator reading the sound file 'file'<br>" +
                             " starting at frame 'start' in channel 'channel' and reading forward if 'direction' is not -1";

var sndclm_make_sampletofile_tip = "<code>(make-sample-&gt;file filename chans data-format header-type comment)</code>:<br>" +
                                   " return an output generator writing the sound file 'filename'<br>" +
                                   " which is set up to have 'chans' channels of 'data-format' samples with a header of 'header-type'.<br>" +
                                   " The latter should be sndlib identifiers:<br>" +
                                   "<code>   (make-sample-&gt;file \"test.snd\" 2 mus-lshort mus-riff)</code>";

var sndclm_make_src_tip = "<code>(make-src :input (:srate 1.0) (:width 10))</code>: return a new sampling-rate conversion generator<br>" +
                          " (using 'warped sinc interpolation').<br>" +
                          " 'srate' is the ratio between the new rate and the old.<br>" +
                          " 'width' is the sine width (effectively the steepness of the low-pass filter), normally between 10 and 100.<br>" +
                          " 'input' if given is an open file stream.";

var sndclm_make_triangle_wave_tip = "<code>(make-triangle-wave (:frequency *clm-default-frequency*) (:amplitude 1.0) (:initial-phase 0.0))</code>:<br>" +
                                    " return a new triangle-wave generator.";

var sndclm_make_two_zero_tip = "<code>(make-two-zero :a0 :a1 :a2 or :frequency :radius)</code>:<br>" +
                               " return a new two-zero filter; a0*x(n) + a1*x(n-1) + a2*x(n-2)";

var sndclm_moving_average_tip = "<code>(moving-average gen (val 0.0))</code>: moving window moving_average.";

var sndclm_mus_close_tip = "<code>(mus-close gen)</code>: close the IO stream managed by 'gen' (a sample-&gt;file generator, for example)";

var sndclm_mus_data_tip = "<code>(mus-data gen)</code>: gen's internal data (a vct)";

var sndclm_mus_frequency_tip = "<code>(mus-frequency gen)</code>: gen's frequency (Hz)";

var sndclm_mus_increment_tip = "<code>(mus-increment gen)</code>: gen's mus-increment field";

var sndclm_mus_length_tip = "<code>(mus-length gen)</code>: gen's length";

var sndclm_mus_offset_tip = "<code>(mus-offset gen)</code>: gen's offset";

var sndclm_mus_scaler_tip = "<code>(mus-scaler gen)</code>: gen's scaler, if any.<br>" +
                            "  This is often an amplitude adjustment of some sort.";

var sndclm_mussrate_tip = "<code>(mus-srate)</code>: current sampling rate";

var sndclm_ncos_tip = "<code>(ncos gen (fm 0.0))</code>: get the next sample from 'gen', an ncos generator";

var sndclm_oscil_tip = "<code>(oscil gen (fm 0.0) (pm 0.0))</code>:<br>" +
                       " next sample from oscil gen: val = sin(phase + pm); phase += (freq + fm)";

var sndclm_out_any_tip = "<code>(out-any frame val chan stream)</code>: add val to output stream at frame in channel chan";

var sndclm_outa_tip = "<code>(outa frame val stream)</code>: add val to output stream at frame in channel 0";

var sndclm_outb_tip = "<code>(outb frame val stream)</code>: add val to output stream at frame in channel 1 (counting from 0)";

var sndclm_output_tip = "<code>*output*</code> is the direct signal output stream.  The reverb input is sent to *reverb*.";

var sndclm_partialstopolynomial_tip = "<code>(partials-&gt;polynomial partials (kind mus-chebyshev-first-kind))</code>:<br>" +
                                      " produce a Chebyshev polynomial suitable for use with the polynomial generator<br>" +
                                      " to create (via waveshaping) the harmonic spectrum described by the partials argument:<br>" +
                                      "<code>  (let ((v0 (partials-&gt;polynomial '(1 1.0 2 1.0)))<br>        (os (make-oscil)))<br>    (polynomial v0 (oscil os)))</code>";

var sndclm_phase_vocoder_tip = "<code>(phase-vocoder gen input-function analyze-func edit-func synthesize-func)</code>: next phase vocoder value";

var sndclm_polynomial_tip = "<code>(polynomial coeffs x)</code>: evaluate a polynomial at x.<br>" +
                            " coeffs are in order of degree, so coeff[0] is the constant term.";

var sndclm_polyshape_tip = "<code>(polyshape gen (index 1.0) (fm 0.0))</code>:<br>" +
                           " next sample of polynomial-based waveshaper";

var sndclm_polywave_tip = "<code>(polywave gen (fm 0.0))</code>:<br>" +
                          " next sample of polynomial-based waveshaper (additive synthesis)";

var sndclm_pulse_train_tip = "<code>(pulse-train gen (fm 0.0))</code>: next pulse train sample from generator";

var sndclm_rand_interp_tip = "<code>(rand-interp gen (fm 0.0))</code>: gen's current (interpolating) random number.<br>" +
                             " fm modulates the rate at which new segment end-points are chosen.";

var sndclm_rand_tip = "<code>(rand gen (fm 0.0))</code>: gen's current random number.<br>" +
                      " fm modulates the rate at which the current number is changed.";

var sndclm_readin_tip = "<code>(readin gen)</code>: next sample from readin generator (a sound file reader)";

var sndclm_reverb_tip = "<code>*reverb*</code> is the reverb stream.  The direct signal is sent to *output*.";

var sndclm_secondstosamples_tip = "<code>(seconds-&gt;samples secs)</code>: use mus-srate to convert seconds to samples";

var sndclm_spectrum_tip = "<code>(spectrum rdat idat window norm-type</code>: return the spectrum of rdat and idat.<br>" +
                          "norm-type defaults to linear (1); the other choices are raw (unnormalized: 2), and dB (0).";

var sndclm_src_tip = "<code>(src gen (pm 0.0) input-function)</code>: next sampling rate conversion sample.<br>" +
                     " 'pm' can be used to change the sampling rate on a sample-by-sample basis.<br>" +
                     " 'input-function' is a function of one argument (the current input direction, normally ignored)<br>" +
                     " that is called internally whenever a new sample of input data is needed.<br>" +
                     " If the associated make-src included an 'input' argument, input-function is ignored.";

var sndclm_tap_tip = "<code>(tap gen (pm 0.0))</code>: tap the delay generator offset by pm";

var sndclm_timestosamples_tip = "<code>(times-&gt;samples beg dur)</code>: returns a list of beg and beg+dur in samples.";

var sndclm_triangle_wave_tip = "<code>(triangle-wave gen (fm 0.0))</code>: next triangle wave sample from generator";

var sndscm_IIRfilters_tip = "These are simple 2nd order IIR filters in dsp.scm.";

var sndscm_analogfilterdoc_tip = "These are the standard 'analog' IIR filters: Butterworth, Chebyshev, etc.";

var sndscm_channelproperty_tip = "<code>(channel-property key snd chn)</code>: returns the value associated with 'key'<br>" +
                                 " in the given channel's property list. To add or change a property,<br>" +
                                 " use set! with this procedure.<br><br>" +
                                 "<code>  (set! (channel-property 'info 0 0) \"this is sound 0, first channel\")</code><br>" +
                                 " now <code>(channel-property 'info 0 0)</code> returns \"this is sound 0, first channel\".";

var sndscm_definstrument_tip = "definstrument is very much like define, but with added code to support notehook<br>" +
                               " and (for Common Music) *definstrument-hook*.";

var sndscm_envelopeinterp_tip = "<code>(envelope-interp x env (base 1.0)</code>: returns value of 'env' at 'x'.<br>" +
                                " If 'base' is 0, 'env' is treated as a step function;<br>" +
                                " if 'base' is 1.0 (the default), the breakpoints of 'env' are connected by a straight line,<br>" +
                                " and any other 'base' connects the breakpoints with an exponential curve.";

var sndscm_envelopelastx_tip = "<code>(envelope-last-x env)</code>: returns the last breakpoint's x axis value in 'env'";

var sndscm_envexptchannel_tip = "<code>(env-expt-channel env exponent (symmetric #t) beg dur snd chn edpos)</code>:<br>" +
                                " applies 'env' to the given channel using 'exponent' for the exponential base.<br>" +
                                " The 'symmetric' argument determines whether the up and down moving ramps look<br>" +
                                " symmetrical around a break point.";

var sndscm_fmviolin_tip = "The fm-violin instrument uses FM to produce a string-like sound;<br>" +
                          " It has many parameters, the principal ones being <code>startime dur frequency amplitude</code>.<br>" +
                          " The code is in v.scm.";

var sndscm_hilberttransform_tip = "<code>(hilbert-transform gen input)</code> returns the Hilbert transform of 'input'.";

var sndscm_html_function_tip = "<code>(html arg)</code> where 'arg' can be a string, a symbol, or a procedure<br>" +
                      " sends the html reader to the corresponding url in the Snd documents.";

var sndscm_insertchannel_tip = "<code>(insert-channel filedat beg dur snd chn edpos)</code>:<br>" +
              " inserts the specified data ('filedat') in the given channel at the given location.<br>" +
              " 'filedat' can be either a filename (a string), a sound index, or a list containing<br>" +
              " the filename (or index), the start point in the file, and (optionally) the channel of the file to mix.";

var sndscm_makebandpass_tip = "<code>(make-bandpass flo fhi length)</code> returns a bandpass filter.";

var sndscm_makebiquad_tip = "<code>(make-biquad a0 a1 a2 b1 b2)</code> returns a biquad filter section.";

var sndscm_makebutter_tip = "various 2nd order Butterworth filters in dsp.scm.";

var sndscm_makedifferentiator_tip = "<code>(make-differentiator length)</code> returns a differentiating filter.";

var sndscm_makeframereader_tip = "<code>(make-frame-reader start snd dir pos)</code>: creates a frame-reader<br>" +
                                 " reading the sound 'snd' starting at frame 'start'<br>" +
                                 " with initial read direction 'dir' (1=forward, -1=backward).<br>" +
                                 " 'pos' is the edit history position to read (it defaults to current position).";

var sndscm_makehighpass_tip = "<code>(make-highpass fc length)</code> returns a highpass filter.";

var sndscm_makehilberttransform_tip = "<code>(make-hilbert-transform length)</code> returns a Hilbert transformer.";

var sndscm_makelowpass_tip = "<code>(make-lowpass fc length)</code> returns a lowpass filter.";

var sndscm_makeramp_tip = "<code>(make-ramp (size 128))</code>: return a ramp generator.";

var sndscm_makeselection_tip = "<code>(make-selection beg end snd chn)</code>: makes a selection,<br>" +
                               " like make-region but without creating a region.<br>" +
                               " It selects 'dur' samples starting at 'beg' in the given channel.";

var sndscm_makespencerfilter_tip = "<code>(make-spencer-filter)</code> returns an FIR filter with the Spencer (smoothing) coefficients.";

var sndscm_markproperties_tip = "<code>(mark-properties id)</code> accesses the property list associated with the mark 'id'";

var sndscm_maxenvelope_tip = "<code>(max-envelope env)</code>: return the maximum y value in 'env'";

var sndscm_moogfilter_tip = "<code>(moog-filter gen input)</code>: return Moog-style 4-pole lowpass filtering of 'input'";

var sndscm_mpg_tip = "<code>(mpg mpgfile rawfile)</code>: call mpg123 to translate an MPEG format sound file<br>" +
                     " to a headerless (\"raw\") file containing 16-bit samples.";

var sndscm_musmix_tip = "<code>(mus-mix outfile infile (outloc 0) (frames) (inloc 0) mixer envs)</code>:<br>" +
                        " mix 'infile' into 'outfile' starting at 'outloc' in 'outfile'<br>" +
                        " and 'inloc' in 'infile', mixing 'frames' frames into 'outfile'.<br>" +
                        " 'frames' defaults to the length of 'infile'.<br>" +
                        " If 'mixer', use it to scale the various channels;<br>" +
                        " if 'envs' (an array of envelope generators), use it in conjunction with mixer<br>" +
                        " to scale and envelope all the various ins and outs.<br>" +
                        " 'outfile' can also be a frame-&gt;file generator, and<br>" +
                        " 'infile' can be a file-&gt;frame generator.";

var sndscm_poly_times_tip = "<code>(poly* p1 p2)</code> multiplies p1 by p2, both polynomials.";

var sndscm_powerenv_tip = "<code>(power-env env)</code>: an envelope generator where each segment has its own base.";

var sndscm_prc95doc_tip = "various physical modeling functions from Perry Cook.";

var sndscm_rmsgain_tip = "various RMS-related generators.";

var sndscm_scalemixes_tip = "<code>(scale-mixes mix-list scl)</code>: scales the amplitude of each mix in 'mix-list' by 'scl'.";

var sndscm_sgfilter_tip = "<code>(savitzky-golay-filter gen input)</code>: a Savitzky-Golay filter, assuming symmetrical positioning.<br>" +
                          " It is an FIR smoothing filter.";

var sndscm_sound_let_tip = "sound-let is a form of let* that creates temporary sound files within with-sound.<br>" +
                       " Its syntax is a combination of let* and with-sound:<br><br>" +
                       "<code> (sound-let ((temp-1 () (fm-violin 0 1 440 .1))<br>" +
                       "             (temp-2 () (fm-violin 0 2 660 .1)<br>" +
                       "                        (fm-violin .125 .5 880 .1)))<br>" +
                       "   (granulate-sound temp-1 0 2 0 2)     ;temp-1 is the name of the 1st temporary file<br>" +
                       "   (granulate-sound temp-2 1 1 0 2))</code><br><br>" +
                       " This creates two temporary files and passes them along to the subsequent calls<br>" +
                       " on granulate-sound.  The first list after the sound file identifier is the list of <br>" +
                       " with-sound options to be passed along when creating this temporary file.  These default<br>" +
                       " to :output with a unique name generated internally, and all other variables are taken from<br>" +
                       " the overall (enclosing) with-sound.  The rest of the list is the body of the associated with-sound.";

var sndscm_sounddatatosound_tip = "<code>(sound-data-&gt;sound sd beg dur snd)</code>: place the contents of<br>" +
                                  " its sound-data argument 'sd' into the sound 'snd' starting at 'beg' and going for 'dur' frames.<br>" +
                                  " 'dur' defaults to the sound-data object's length.";

var sndscm_soundinterp_tip = "<code>(sound-interp reader loc)</code>: the sound-interp interpolating reader<br>" +
                             " reads a channel at an arbitary location, interpolating between samples if necessary.";

var sndscm_soundtosounddata_tip = "<code>(sound-&gt;sound-data beg dur snd)</code>:<br>" +
                                  " return a sound-data object containing the contents of the sound 'snd'<br>" +
                                  " starting from beg for dur frames.<br>" +
                                  " <code>  (sound-data-&gt;sound (sound-data* (sound-&gt;sound-data) 2.0))</code><br>" +
                                  " is yet another way to scale a sound by 2.0.";

var sndscm_syncdmixes_tip = "<code>(syncd-mixes sync)</code>:  returns a list of all mixes whose mix-sync field is set to 'sync'.";

var sndscm_tofrequency_tip = "<code>(-&gt;frequency pitch ratio)</code> takes either a number or a common-music pitch symbol<br>" +
                       " ('c4 is middle C), and returns either the number or the frequency associated with that pitch:<br>" +
                       " <code>(-&gt;frequency 'cs5)</code> returns 554 and change.<br>" +
                       " 'ratio' can be #t to get small integer ratios rather than equal temperment.";

var sndscm_tosample_tip = "<code>(-&gt;sample time)</code> returns a sample number given a time in seconds";

var sndscm_volterrafilter_tip = "<code>(volterra-filter flt x)</code>: pass 'x' through the Volterra (non-linear) filter 'flt'.";

var sndscm_windowsamples_tip = "<code>(window-samples snd chn)</code>: returns (in a vct) the samples<br>" +
                               " displayed in the current graph window for the given channel.";

var sndscm_withtempsound_tip = "with-temp-sound is like sound-let (it sets up a temporary output<br>" +
                               " for with-sound) , but does not delete its output file.";

var sndscm_wsdoc_tip = "with-sound provides a simple way to package up a bunch of instrument calls into a new<br>" +
                       " sound file, and open that file in Snd when the computation is complete. <br>" +
                       " with-sound opens an output object, and optionally a reverb output object.<br>" +
                       " Each instrument uses out-any to add its sounds to the *output* results.<br>" +
                       "<pre> with-sound<br>" + 
                       "       (srate *clm-srate*)             ; output sampling rate (44100)<br>" + 
                       "       (output *clm-file-name*)        ; output file name (\"test.snd\")<br>" + 
                       "       (channels *clm-channels*)       ; channels in output (1)<br>" + 
                       "       (header-type *clm-header-type*) ; output header type (mus-next or mus-aifc)<br>" + 
                       "       (data-format *clm-data-format*) ; output sample data type (mus-bfloat)<br>" + 
                       "       (comment #f)                    ; any comment to store in the header (a string)<br>" + 
                       "       (reverb *clm-reverb*)           ; reverb instrument (jc-reverb)<br>" + 
                       "       (reverb-data *clm-reverb-data*) ; arguments passed to the reverb<br>" + 
                       "       (statistics *clm-statistics*)   ; if #t, print info at end of with-sound<br>" + 
                       "       (scaled-to #f)                  ; if a number, scale the output to peak at that amp<br>" + 
                       "       (play *clm-play*)               ; if #t, play the sound automatically</pre><br>" + 
                       " The with-sound syntax may look sightly odd; we include the arguments in the<br>" +
                       " first list, then everything after that is evaluated as a note list.<br>" +
                       "<pre>   (with-sound (:srate 44100 :channels 2 :output \"test.snd\")<br>" +
                       "      (fm-violin 0 1 440 .1)<br>" +
                       "      (fm-violin 1 1 660 .1))</pre><br>" +
                       " produces a sound file with two fm-violin notes; the sound file is named \"test.snd\",<br>" +
                       " is stero, and has a sampling rate of 44100.";

var sndscm_zipper_tip = "<code>(zipper gen in1 in2)</code>: the digital zipper; a way to crossfade between in1 and in2.";



var sndlib_html_tip = "library that handles sound files and audio ports";

var sndclm_html_tip = "sound synthesis generators";

var sndscm_html_tip = "Scheme, Ruby, and Forth files included with Snd";

var fm_html_tip = "introduction to frequency modulation";

var extsnd_html_tip = "Snd extension and customization";

var grfsnd_html_tip = "Snd configuration, connection to other libraries and programs";

var snd_html_tip = "basic Snd user-interface documentation";

var libxm_html_tip = "library that ties Motif and Gtk into Snd";

var s7_html_tip = "a Scheme implementation that comes with Snd";

var index_html_tip = "overall index";



var analog_filter_doc_tip = "These are the traditional IIR filters, any type, any even order<br>" +
                            "(Butterworth, Chebyshev, Inverse Chebyshev, Bessel, and Elliptic)<br>" +
                            "The elliptic function filters need GSL.";

var animals_doc_tip = "synthesis of birds, frogs, and insects";

var autosave_doc_tip = "periodically save current sound edits in a backup file, <br>" +
                       "useful if your machine crashes a lot.";

var bess_doc_tip = "This sets up a dialog to experiment with simple FM, <br>" +
                   "the fm-violin, or with a compositional algorithm";

var binary_io_doc_tip = "This file has functions to read and write binary files";

var bird_doc_tip = "simple synthesis of about 50 birds using additive synthesis.<br>" +
                   "see animals.scm for much more elaborate versions of these birds";

var clean_doc_tip = "click, pop, and hum removal, and signal reconstruction";

var clm_ins_doc_tip = "Instruments using many standard synthesis techniques,<br>" +
                      " including a bagpipe, FOF synthesis, many FM examples,<br>" +
                      " granular synthesis, spectral modeling, reverbs, and physical modeling.";


var dlocsig_doc_tip = "dlocsig sets up envelopes to mimic a moving sound;<br>" +
                      " included are many path-specification functions";

var draw_doc_tip = "Examples of drawing extensions, primarily one that puts a thumbnail graph<br>" +
                   " of the current sound in the upper right corner";

var dsp_doc_tip = "This has all the usual DSP stuff: filters, ffts, sample rate conversion, <br>" +
                  " sound effects, statistics, scanned synthesis, transforms, etc";

var env_doc_tip = "Various operations on envelopes: add, scale, copy, stretch";

var enved_doc_tip = "This adds an envelope editor to each displayed channel.<br>" +
                    " You can set it up to be an amplitude envelope.";

var examp_doc_tip = "A bunch of examples of things like ffts, filters, marks, selections,<br>" +
                    " graphics extensions, sound effects, and generators.";

var extensions_doc_tip = "channel and sound property lists, several enveloping functions,<br>" +
                         " and commonly used editing sequences such as channel extraction.";

var fade_doc_tip = "sound mixing using envelopes in the frequency domain";

var frame_doc_tip = "various frame, vct, and sound-data functions";

var freeverb_doc_tip = "a reverberator along the lines of nrev, but with more options.";

var generators_doc_tip = "about 80 generators related to sums of sinusoids<br>" +
                         " bessel functions, adjustable square-waves, special envelopes, etc";

var grani_doc_tip = "this is a very flexible granular synthesis instrument";

var heart_doc_tip = "This code is aimed at blood pressure readings.";

var hooks_doc_tip = "snd-hooks, describe-hook, with-local-hook, reset-all-hooks.";

var index_doc_tip = "this provides a connection between firefox and the snd-help mechanism.";

var inf_snd_doc_tip = "this provides a Snd emacs mode implementation.<br>" +
                      "  You can use emacs as the listener, rather than the built-in Snd window.";

var jcrev_doc_tip = "this is probably the first Schroeder reverb, based on all-pass and comb filters.";

var maraca_doc_tip = "this includes the maraca, tambourine, wind-chimes, etc";

var marks_doc_tip = "this includes describe-mark, eval-between-marks, mark-property,<br>" +
                    " play-between-marks, and snap-marks.";

var maxf_doc_tip = "This is a collection of modal synthesis demos.<br>" +
                   "  For the actual filter, see the firmant generator";

var menus_doc_tip = "Menu additions for things like crop, trim, fft notch filter,<br>" +
                    " mark and mix functions, etc.  The main added menu loads a huge<br>" +
                    " set of sound effects";

var mix_doc_tip = "mix-property, silence-all-mixes, mix-sound, save-mix, snap-mix-to-beat<br>" +
                  " and many functions acting on lists of mixes";

var mixer_doc_tip = "mixers and frames treated as matrices and vectors: <br>" +
                    " matrix determinant, transpose, invert, solve, mixer-poly, etc";

var moog_doc_tip = "Moog's four pole lowpass (24db/Oct) filter as a clm generator,<br>" +
                   " variable resonance, \"that warm, analog sound\".";

var musglyphs_doc_tip = "The CMN music symbol font built from bezier curves.<br>" +
                        "This file is a lisp-&gt;scheme wrapper for cmn-glyphs.lisp";

var nb_doc_tip = "As you move the mouse through the view-files list,<br>" +
                 " the help dialog posts information about the file underneath the mouse";

var noise_doc_tip = "This ancient noise instrument can produce those all-important whooshing<br>" +
                    " sounds.  noise.ins translated to Scheme/Ruby by Michael Scholz";

var numerics_doc_tip = "Various numerical functions: factorial, plgndr, gegenbaur, etc";

var oscope_doc_tip = "oscope.scm sets up a dialog with a Snd channel window<br>" +
                     " (time domain, fft etc) that displays data read from the<br>" +
                     " microphone in real time.";

var piano_doc_tip = "Scott van Duyne's piano model that includes multiple coupled strings,<br>" +
                    " a nonlinear hammer, and an arbitrarily large soundboard and enclosure";

var play_doc_tip = "play between marks, play continuously, play a set of sines, etc";

var poly_doc_tip = "polynomial addition, multiplication, division, gcd, roots, and discriminant";

var prc95_doc_tip = "The basic physical models: pluck, bow, clarinet, brass, flute";

var pvoc_doc_tip = "various versions of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder.<br>" +
                   "see also the CLM phase-vocoder generator.";

var rgb_doc_tip = "this translates the standard X11 color names into Snd color objects.";

var rubber_doc_tip = "rubber-sound tries to stretch or contract a sound (in time);<br>" +
                     " it scans the sound looking for stable sections, then either <br>" +
                     " deletes periods or interpolates new ones to shorten or lengthen the sound";

var selection_doc_tip = "includes swap-selection-channels, replace-with-selection, <br>" +
                        " selection-members, make-selection, delete-selection-and-smooth,<br>" +
                        " filter-selection-and-smooth, and with-temporary-selection";

var singer_doc_tip = "This is based on Perry's singer.c and CLM's singer.ins";

var sndold_doc_tip = "These files (snd10.scm to snd12.scm) provide backwards compatibility<br>" +
                   " with earlier versions of Snd.";

var snddiff_doc_tip = "a diff or grep-like function for sounds. It can currently find<br>" +
                      " initial delays, scaling differences, and scattered individual<br>" +
                      " sample differences: <code>(snddiff snd0 chn0 snd1 chn1)</code>.";

var snd_gl_doc_tip = "This depends on access to openGL (Mesa); it includes a waterfall fft graph<br>" +
                     "and GL state readbacks";

var snd_motif_doc_tip = "user interface extensions using the libxm modules: add-mark-pane,<br>" +
                        " display-scanned-synthesis, load-font, with-level-meters,<br>" + 
                        " variable-display, smpte labels, and lots more.<br>" +  
                        " snd-motif is for Motif, snd-gtk for Gtk.";

var snd_test_doc_tip = "Snd regression test suite; zillions of examples.";

var sndwarp_doc_tip = "time stretching and whatnot";

var ws_doc_tip = "with-sound provides a simple way to package up a bunch of<br>" +
                 " instrument calls into a new sound file,  and open that file in Snd";

var zip_doc_tip = "The zipper marches through the two sounds taking equal short portions<br>" +
                  " of each, then abutting them while resampling so that as one sound<br>" +
                  " takes less overall frame space, the other takes more.";

var scheme_format_tip = "<code>(format destination control-string :rest args)</code> produces formatted output.<br>" +
                        "If 'destination' is #f (the usual case in Snd), the output is a string.<br>" +
                        "The output depends on 'control-string' which can contain characters preceded by tilde.<br>" +
                        "These are replaced with other strings based on the character and the associated argument in 'args'.<br>" +
                        "The main tilde cases are ~% = add a newline, ~A = add some readable description of its argument<br>" +
                        "~D = treat its arg as an integer, ~F = treat arg as float, ~S = treat arg as string.<br><br>" +
                        "<code>(format #f \"A: ~A, D: ~D, F: ~F~%\" (make-vct 2 3.14) 32 1.5)</code><br><br>" +
                        "produces:<br><br>" +
                        "<code>\"A: #&lt;vct[len=2]: 3.140 3.140&gt;, D: 32, F: 1.5<br>" +
                        "\"</code>.";
