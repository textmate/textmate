#include "editor.h"
#include "transform.h"
#include <command/parser.h>
#include <document/collection.h>

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

		for(size_t i = 0; i < sizeofA(actions); ++i)
		{
			if(sel == actions[i].selector)
				return actions[i].action;
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

	void editor_t::execute_dispatch (plist::dictionary_t const& plist, std::map<std::string, std::string> const& variables)
	{
		document::run(parse_command(convert_command_from_v1(plist)), _buffer, _selections, _document, variables);
	}

	void editor_t::macro_dispatch (plist::dictionary_t const& plist, std::map<std::string, std::string> const& variables)
	{
		plist::array_t commands;
		if(!plist::get_key_path(plist, "commands", commands))
			return;

		// // TODO macro: useGlobalClipboard
		// bool useGlobalClipboard = false;
		// plist::get_key_path(plist, "useGlobalClipboard", useGlobalClipboard);

		iterate(command, commands)
		{
			plist::dictionary_t dict = boost::get<plist::dictionary_t>(*command); // TODO ASSERT this!
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
					macro_dispatch(args, variables);
				}
				else if(sel == "insertSnippetWithOptions:" && plist::get_key_path(dict, "argument", args))
				{
					snippet_dispatch(args, variables);
				}
				else if(sel == "executeCommandWithOptions:" && plist::get_key_path(dict, "argument", args))
				{
					execute_dispatch(args, variables);
				}
				else
				{
					ASSERTF(to_action(sel) != kNop, "%s", sel.c_str());
					perform(to_action(sel));
				}
			}
		}
	}

} /* ng */
