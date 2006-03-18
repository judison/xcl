object FrmCompilerOpts: TFrmCompilerOpts
  Title = 'Compiler Options'
  BorderWidth = 3
  object TVBox
    Spacing = 4
    object TNotebook
      ShowTabs = True
      BoxExpand = True
      object TNotebookPage
        Caption = 'Syntax'
        object TFrame
          Caption = 'Syntax Switches'
          object THBox
            object TVBox
              BoxExpand = True
              object TCheckButton
                Caption = 'Object Pascal support'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'C-like operators'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Stop after first error'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Allow LABEL and GOTO'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'C++ style inline'
                BoxExpand = True
              end
            end
            object TVBox
              BoxExpand = True
              object TCheckButton
                Caption = 'Global C macros'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'TP/BP 7.0 compatibility'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Delphi compatibility'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Allow STATIC in objects'
                BoxExpand = True
              end
            end
          end
        end
      end
      object TNotebookPage
        Caption = 'Code generation'
        object THBox
          object TVBox
            BoxExpand = True
            object TFrame
              Caption = 'Run-Time checks'
              BoxExpand = True
              object TVBox
                object TCheckButton
                  Caption = 'Range checking'
                  BoxExpand = True
                end
                object TCheckButton
                  Caption = 'Stack checking'
                  BoxExpand = True
                end
                object TCheckButton
                  Caption = 'I/O checking'
                  BoxExpand = True
                end
                object TCheckButton
                  Caption = 'Integer overflow checking'
                  BoxExpand = True
                end
              end
            end
            object TFrame
              Caption = 'Target processor'
              BoxExpand = True
              object TVBox
                object TRadioButton
                  Caption = 'i386/i486'
                  BoxExpand = True
                end
                object TRadioButton
                  Caption = 'Pentium/PentiumMMX (tm)'
                  BoxExpand = True
                end
                object TRadioButton
                  Caption = 'PPro/PII/c6x86/K6 (tm)'
                  BoxExpand = True
                end
              end
            end
          end
          object TFrame
            Caption = 'Optimizations'
            BoxExpand = True
            object TVBox
              object TCheckButton
                Caption = 'Generate faster code'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Generate smaller code'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Use register-variables'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Uncertain optimizations'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Level 1 optimizations'
                BoxExpand = True
              end
              object TCheckButton
                Caption = 'Level 2 optimizations'
                BoxExpand = True
              end
            end
          end
        end
      end
      object TNotebookPage
        Caption = 'Verbose'
        object TFrame
          Caption = 'Verbose Switches'
          object TVBox
            object TCheckButton
              Caption = 'Warnings'
              BoxExpand = True
            end
            object TCheckButton
              Caption = 'Notes'
              BoxExpand = True
            end
            object TCheckButton
              Caption = 'Hints'
              BoxExpand = True
            end
            object TCheckButton
              Caption = 'General Info'
              BoxExpand = True
            end
            object TCheckButton
              Caption = 'Used,tried info'
              BoxExpand = True
            end
            object TCheckButton
              Caption = 'All'
              BoxExpand = True
            end
            object TCheckButton
              Caption = 'Show all Procedures if error'
              BoxExpand = True
            end
          end
        end
      end
      object TNotebookPage
        Caption = 'Browser'
        object TFrame
          Caption = 'Browser'
          object TVBox
            object TRadioButton
              Caption = 'No browser'
              BoxExpand = True
            end
            object TRadioButton
              Caption = 'Only Global browser'
              BoxExpand = True
            end
            object TRadioButton
              Caption = 'Local and global browser'
              BoxExpand = True
            end
          end
        end
      end
      object TNotebookPage
        Caption = 'Assembler'
        object THBox
          object TVBox
            BoxExpand = True
            object TFrame
              Caption = 'Assembler reader'
              BoxExpand = True
              object TVBox
                object TRadioButton
                  Caption = 'Direct assembler'
                  BoxExpand = True
                end
                object TRadioButton
                  Caption = 'AT&T style assembler'
                  BoxExpand = True
                end
                object TRadioButton
                  Caption = 'Intel style assembler'
                  BoxExpand = True
                end
              end
            end
            object TFrame
              Caption = 'Assembler info'
              BoxExpand = True
              object TVBox
                object TCheckButton
                  Caption = 'List source'
                  BoxExpand = True
                end
                object TCheckButton
                  Caption = 'List register allocation'
                  BoxExpand = True
                end
                object TCheckButton
                  Caption = 'List temp allocation'
                  BoxExpand = True
                end
              end
            end
          end
          object TFrame
            Caption = 'Assembler output'
            BoxExpand = True
            object TVBox
              object TRadioButton
                Caption = 'Use default output'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use GNU as'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use NASM coff'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use NASM elf'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use NASM obj'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use MASM'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use TASM'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use coff'
                BoxExpand = True
              end
              object TRadioButton
                Caption = 'Use pecoff'
                BoxExpand = True
              end
            end
          end
        end
      end
    end
    object TLabel
      Caption = 'Conditional defines'
      BoxExpand = True
    end
    object TEntry
      BoxExpand = True
    end
    object THSeparator
    end
    object THButtonBox
      Layout = bblEnd
      Spacing = 8
      BoxExpand = True
      object TButton
        Caption = '_Cancel'
        IconName = 'gtk-cancel'
        OnClicked = BtnCancelClicked
        BoxExpand = True
      end
      object TButton
        Caption = '_OK'
        IconName = 'gtk-ok'
        OnClicked = BtnOKClicked
        BoxExpand = True
      end
    end
  end
end
