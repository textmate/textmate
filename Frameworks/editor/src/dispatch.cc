#include "editor.h"
#include "transform.h"
#include "clipboard.h"
#include <command/src/parser.h>

namespace
{
	struct macro_clipboard_t : clipboard_t
	{
		macro_clipboard_t (clipboard_ptr old) : _old(old) { }

		bool empty () const               { return _stack.empty(); }

		entry_ptr previous ()             { return _index == 0               ? entry_ptr() : _stack[--_index]; }
		entry_ptr current () const        { return _index == _stack.size()   ? entry_ptr() : _stack[  _index]; }
		entry_ptr next ()                 { return _index+1 >= _stack.size() ? entry_ptr() : _stack[++_index]; }

		void push_back (entry_ptr entry)
		{
			_stack.push_back(entry);
			_index = _stack.size()-1;
		}

		clipboard_ptr _old;

	private:
		std::vector<entry_ptr> _stack;
		size_t _index = 0;
	};

	clipboard_ptr create_macro_clipboard (clipboard_ptr old)
	{
		clipboard_ptr res = std::make_shared<macro_clipboard_t>(old);
		if(old && old->current())
			res->push_back(old->current());
		return res;
	}
}

namespace ng
{
	action_t to_action (std::string const& sel)
	{
		static struct { std::string selector; action_t action; } const actions[] =
		{
			{ "moveBackward:",                                      kMoveBackward                                 },
			{ "moveBackwardAndModifySelection:",                    kMoveBackwardAndModifySelection               },
			{ "moveDown:",                                          kMoveDown                                     },
			{ "moveDownAndModifySelection:",                        kMoveDownAndModifySelection                   },
			{ "moveForward:",                                       kMoveForward                                  },
			{ "moveForwardAndModifySelection:",                     kMoveForwardAndModifySelection                },
			{ "moveParagraphBackwardAndModifySelection:",           kMoveParagraphBackwardAndModifySelection      },
			{ "moveParagraphForwardAndModifySelection:",            kMoveParagraphForwardAndModifySelection       },
			{ "moveSubWordLeft:",                                   kMoveSubWordLeft                              },
			{ "moveSubWordLeftAndModifySelection:",                 kMoveSubWordLeftAndModifySelection            },
			{ "moveSubWordRight:",                                  kMoveSubWordRight                             },
			{ "moveSubWordRightAndModifySelection:",                kMoveSubWordRightAndModifySelection           },
			{ "moveToBeginningOfColumn:",                           kMoveToBeginningOfColumn                      },
			{ "moveToBeginningOfColumnAndModifySelection:",         kMoveToBeginningOfColumnAndModifySelection    },
			{ "moveToBeginningOfDocument:",                         kMoveToBeginningOfDocument                    },
			{ "moveToBeginningOfDocumentAndModifySelection:",       kMoveToBeginningOfDocumentAndModifySelection  },
			{ "moveToBeginningOfIndentedLine:",                     kMoveToBeginningOfIndentedLine                   },
			{ "moveToBeginningOfIndentedLineAndModifySelection:",   kMoveToBeginningOfIndentedLineAndModifySelection },
			{ "moveToEndOfIndentedLine:",                           kMoveToEndOfIndentedLine                      },
			{ "moveToEndOfIndentedLineAndModifySelection:",         kMoveToEndOfIndentedLineAndModifySelection    },
			{ "moveToBeginningOfLine:",                             kMoveToBeginningOfLine                        },
			{ "moveToBeginningOfLineAndModifySelection:",           kMoveToBeginningOfLineAndModifySelection      },
			{ "moveToBeginningOfParagraph:",                        kMoveToBeginningOfParagraph                   },
			{ "moveToBeginningOfParagraphAndModifySelection:",      kMoveToBeginningOfParagraphAndModifySelection },
			{ "moveToBeginningOfBlock:",                            kMoveToBeginningOfBlock                       },
			{ "moveToBeginningOfBlockAndModifySelection:",          kMoveToBeginningOfBlockAndModifySelection     },
			{ "moveToEndOfColumn:",                                 kMoveToEndOfColumn                            },
			{ "moveToEndOfColumnAndModifySelection:",               kMoveToEndOfColumnAndModifySelection          },
			{ "moveToEndOfDocument:",                               kMoveToEndOfDocument                          },
			{ "moveToEndOfDocumentAndModifySelection:",             kMoveToEndOfDocumentAndModifySelection        },
			{ "moveToEndOfLine:",                                   kMoveToEndOfLine                              },
			{ "moveToEndOfLineAndModifySelection:",                 kMoveToEndOfLineAndModifySelection            },
			{ "moveToEndOfParagraph:",                              kMoveToEndOfParagraph                         },
			{ "moveToEndOfParagraphAndModifySelection:",            kMoveToEndOfParagraphAndModifySelection       },
			{ "moveToEndOfBlock:",                                  kMoveToEndOfBlock                             },
			{ "moveToEndOfBlockAndModifySelection:",                kMoveToEndOfBlockAndModifySelection           },
			{ "moveUp:",                                            kMoveUp                                       },
			{ "moveUpAndModifySelection:",                          kMoveUpAndModifySelection                     },
			{ "moveWordBackward:",                                  kMoveWordBackward                             },
			{ "moveWordBackwardAndModifySelection:",                kMoveWordBackwardAndModifySelection           },
			{ "moveWordForward:",                                   kMoveWordForward                              },
			{ "moveWordForwardAndModifySelection:",                 kMoveWordForwardAndModifySelection            },

			{ "moveLeft:",                                          kMoveBackward                                 },
			{ "moveLeftAndModifySelection:",                        kMoveBackwardAndModifySelection               },
			{ "moveRight:",                                         kMoveForward                                  },
			{ "moveRightAndModifySelection:",                       kMoveForwardAndModifySelection                },
			{ "moveWordLeft:",                                      kMoveWordBackward                             },
			{ "moveWordLeftAndModifySelection:",                    kMoveWordBackwardAndModifySelection           },
			{ "moveWordRight:",                                     kMoveWordForward                              },
			{ "moveWordRightAndModifySelection:",                   kMoveWordForwardAndModifySelection            },
			{ "delete:",                                            kDeleteSelection                              },

			{ "pageDown:",                                          kPageDown                                     },
			{ "pageDownAndModifySelection:",                        kPageDownAndModifySelection                   },
			{ "pageUp:",                                            kPageUp                                       },
			{ "pageUpAndModifySelection:",                          kPageUpAndModifySelection                     },

			{ "selectAll:",                                         kSelectAll                                    },
			{ "selectCurrentScope:",                                kSelectCurrentScope                           },
			{ "selectBlock:",                                       kSelectBlock                                  },
			{ "selectHardLine:",                                    kSelectHardLine                               },
			{ "selectLine:",                                        kSelectLine                                   },
			{ "selectParagraph:",                                   kSelectParagraph                              },
			{ "selectWord:",                                        kSelectWord                                   },
			{ "toggleColumnSelection:",                             kToggleColumnSelection                        },
			{ "deselectLast:",                                      kDeselectLast                                 },

			{ "findNext:",                                          kFindNext                                     },
			{ "findPrevious:",                                      kFindPrevious                                 },
			{ "findNextAndModifySelection:",                        kFindNextAndModifySelection                   },
			{ "findPreviousAndModifySelection:",                    kFindPreviousAndModifySelection               },
			{ "findAll:",                                           kFindAll                                      },
			{ "findAllInSelection:",                                kFindAllInSelection                           },

			{ "replace:",                                           kReplace                                      },
			{ "replaceAll:",                                        kReplaceAll                                   },
			{ "replaceAllInSelection:",                             kReplaceAllInSelection                        },

			{ "replaceAndFind:",                                    kReplaceAndFind                               },

			{ "deleteBackward:",                                    kDeleteBackward                               },
			{ "deleteForward:",                                     kDeleteForward                                },
			{ "deleteSubWordLeft:",                                 kDeleteSubWordLeft                            },
			{ "deleteSubWordRight:",                                kDeleteSubWordRight                           },
			{ "deleteToBeginningOfIndentedLine:",                   kDeleteToBeginningOfIndentedLine              },
			{ "deleteToBeginningOfLine:",                           kDeleteToBeginningOfLine                      },
			{ "deleteToBeginningOfParagraph:",                      kDeleteToBeginningOfParagraph                 },
			{ "deleteToEndOfIndentedLine:",                         kDeleteToEndOfIndentedLine                    },
			{ "deleteToEndOfLine:",                                 kDeleteToEndOfLine                            },
			{ "deleteToEndOfParagraph:",                            kDeleteToEndOfParagraph                       },
			{ "deleteWordBackward:",                                kDeleteWordBackward                           },
			{ "deleteWordForward:",                                 kDeleteWordForward                            },
			{ "deleteBackwardByDecomposingPreviousCharacter:",      kDeleteBackwardByDecomposingPreviousCharacter },
			{ "deleteSelection:",                                   kDeleteSelection                              },

			{ "cut:",                                               kCut                                          },
			{ "copy:",                                              kCopy                                         },
			{ "copySelectionToFindPboard:",                         kCopySelectionToFindPboard                    },
			{ "copySelectionToReplacePboard:",                      kCopySelectionToReplacePboard                 },
			{ "paste:",                                             kPaste                                        },
			{ "pastePrevious:",                                     kPastePrevious                                },
			{ "pasteNext:",                                         kPasteNext                                    },
			{ "pasteWithoutReindent:",                              kPasteWithoutReindent                         },
			{ "yank:",                                              kYank                                         },

			{ "capitalizeWord:",                                    kCapitalizeWord                               },
			{ "changeCaseOfLetter:",                                kChangeCaseOfLetter                           },
			{ "changeCaseOfWord:",                                  kChangeCaseOfWord                             },
			{ "lowercaseWord:",                                     kLowercaseWord                                },
			{ "reformatText:",                                      kReformatText                                 },
			{ "reformatTextAndJustify:",                            kReformatTextAndJustify                       },
			{ "shiftLeft:",                                         kShiftLeft                                    },
			{ "shiftRight:",                                        kShiftRight                                   },
			{ "transpose:",                                         kTranspose                                    },
			{ "transposeWords:",                                    kTransposeWords                               },
			{ "unwrapText:",                                        kUnwrapText                                   },
			{ "uppercaseWord:",                                     kUppercaseWord                                },

			{ "setMark:",                                           kSetMark                                      },
			{ "deleteToMark:",                                      kDeleteToMark                                 },
			{ "selectToMark:",                                      kSelectToMark                                 },
			{ "swapWithMark:",                                      kSwapWithMark                                 },

			{ "complete:",                                          kComplete                                     },
			{ "nextCompletion:",                                    kNextCompletion                               },
			{ "previousCompletion:",                                kPreviousCompletion                           },

			{ "insertBacktab:",                                     kInsertBacktab                                },
			{ "insertTab:",                                         kInsertTab                                    },
			{ "insertTabIgnoringFieldEditor:",                      kInsertTabIgnoringFieldEditor                 },
			{ "insertNewline:",                                     kInsertNewline                                },
			{ "insertNewlineIgnoringFieldEditor:",                  kInsertNewlineIgnoringFieldEditor             },

			{ "indent:",                                            kIndent                                       },

			{ "moveSelectionUp:",                                   kMoveSelectionUp                              },
			{ "moveSelectionDown:",                                 kMoveSelectionDown                            },
			{ "moveSelectionLeft:",                                 kMoveSelectionLeft                            },
			{ "moveSelectionRight:",                                kMoveSelectionRight                           },

			{ "noop:",                                              kNop                                          },
		};

		for(auto const& action : actions)
		{
			if(sel == action.selector)
				return action.action;
		}
		return kNop;
	}

	void editor_t::find_dispatch (plist::dictionary_t const& args)
	{
		bool flag;
		find::options_t options = find::none;
		options = plist::get_key_path(args, "fullWordMatch",     flag) && flag ? options | find::full_words         : options;
		options = plist::get_key_path(args, "ignoreCase",        flag) && flag ? options | find::ignore_case        : options;
		options = plist::get_key_path(args, "ignoreWhitespace",  flag) && flag ? options | find::ignore_whitespace  : options;
		options = plist::get_key_path(args, "regularExpression", flag) && flag ? options | find::regular_expression : options;
		options = plist::get_key_path(args, "wrapAround",        flag) && flag ? options | find::wrap_around        : options;

		std::string action = "findNext", searchFor = NULL_STR, replaceWith = NULL_STR;
		plist::get_key_path(args, "action", action);
		plist::get_key_path(args, "findString", searchFor);
		plist::get_key_path(args, "replaceString", replaceWith);

		if(searchFor != NULL_STR)
		{
			static struct { std::string key; find::options_t flag; } const optionMap[] =
			{
				{ "fullWordMatch",       find::full_words         },
				{ "ignoreCase",          find::ignore_case        },
				{ "ignoreWhitespace",    find::ignore_whitespace  },
				{ "regularExpression",   find::regular_expression },
				{ "wrapAround",          find::wrap_around        },
			};

			std::map<std::string, std::string> searchOptions;
			for(auto const& option : optionMap)
			{
				if(options & option.flag)
					searchOptions.emplace(option.key, "1");
			}

			find_clipboard()->push_back(searchFor, searchOptions);
		}
		else if(clipboard_t::entry_ptr searchEntry = find_clipboard()->current())
		{
			searchFor = searchEntry->content();
			options |= convert(searchEntry->options());
		}

		if(replaceWith != NULL_STR)
			replace_clipboard()->push_back(replaceWith);
		else if(clipboard_t::entry_ptr replaceEntry = replace_clipboard()->current())
			replaceWith = replaceEntry->content();

		std::string where;
		bool searchOnlySelection = plist::get_key_path(args, "replaceAllScope", where) && where == "selection";

		if(action == "replace")
		{
			perform(kReplace);
		}
		else if(action == "replaceAndFind")
		{
			perform(kReplace);
			find(searchFor, options, searchOnlySelection);
		}
		else if(action == "replaceAll")
		{
			replace_all(searchFor, replaceWith, options|find::all_matches, searchOnlySelection);
		}
		else // findNext, findPrevious, and findAll
		{
			if(action == "findPrevious")
				options |= find::backwards;
			else if(action == "findAll")
				options |= find::all_matches;
			find(searchFor, options, searchOnlySelection);
		}
	}

	void editor_t::snippet_dispatch (plist::dictionary_t const& plist, std::map<std::string, std::string> const& variables)
	{
		std::string str;
		if(plist::get_key_path(plist, "content", str))
		{
			bool disableAutoIndent = false;
			plist::get_key_path(plist, "disableAutoIndent", disableAutoIndent);
			snippet(str, variables, disableAutoIndent);
		}
	}

	void editor_t::execute_dispatch (plist::dictionary_t const& plist, std::map<std::string, std::string> const& variables, std::function<void(bundle_command_t const&, ng::buffer_api_t const&, ng::ranges_t const&, std::map<std::string, std::string> const&)> const& runner)
	{
		runner(parse_command(convert_command_from_v1(plist)), _buffer, _selections, variables);
	}

	void editor_t::macro_dispatch (plist::dictionary_t const& plist, std::map<std::string, std::string> const& variables, std::function<void(bundle_command_t const&, ng::buffer_api_t const&, ng::ranges_t const&, std::map<std::string, std::string> const&)> const& runner)
	{
		plist::array_t commands;
		if(!plist::get_key_path(plist, "commands", commands))
			return;

		bool useGlobalClipboard = false;
		plist::get_key_path(plist, "useGlobalClipboard", useGlobalClipboard);

		clipboard_ptr oldClipboard        = clipboard();
		clipboard_ptr oldFindClipboard    = find_clipboard();
		clipboard_ptr oldReplaceClipboard = replace_clipboard();

		if(!useGlobalClipboard)
			set_clipboard(create_macro_clipboard(oldClipboard));
		set_find_clipboard(create_macro_clipboard(oldFindClipboard));
		set_replace_clipboard(create_macro_clipboard(oldReplaceClipboard));

		for(auto const& command : commands)
		{
			plist::dictionary_t dict = boost::get<plist::dictionary_t>(command); // TODO ASSERT this!
			std::string sel, str;
			plist::dictionary_t args;
			if(plist::get_key_path(dict, "command", sel))
			{
				if(sel == "insertText:" && plist::get_key_path(dict, "argument", str))
				{
					insert(str);
				}
				else if(sel == "deleteTabTrigger:" && plist::get_key_path(dict, "argument", str))
				{
					delete_tab_trigger(str);
				}
				else if(sel == "findWithOptions:" && plist::get_key_path(dict, "argument", args))
				{
					find_dispatch(args);
				}
				else if(sel == "playMacroWithOptions:" && plist::get_key_path(dict, "argument", args))
				{
					macro_dispatch(args, variables, runner);
				}
				else if(sel == "insertSnippetWithOptions:" && plist::get_key_path(dict, "argument", args))
				{
					snippet_dispatch(args, variables);
				}
				else if(sel == "executeCommandWithOptions:" && plist::get_key_path(dict, "argument", args))
				{
					execute_dispatch(args, variables, runner);
				}
				else
				{
					ASSERTF(to_action(sel) != kNop, "%s", sel.c_str());
					perform(to_action(sel));
				}
			}
		}

		if(!useGlobalClipboard)
			set_clipboard(oldClipboard);
		set_find_clipboard(oldFindClipboard);
		set_replace_clipboard(oldReplaceClipboard);
	}

} /* ng */
