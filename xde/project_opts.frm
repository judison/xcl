object FrmProjectOpts: TFrmProjectOpts
  Title = 'Project Options'
  BorderWidth = 3
  object TVBox
    Spacing = 4
    object TNotebook
      BoxExpand = True
      object TNotebookPage
        Caption = 'Directories/Conditionals'
        object TTable
          NCols = 3
          NRows = 5
          BorderWidth = 2
          object TLabel
            Caption = '_Output Path'
            UseUnderline = True
            XAlign = 0
            TableRightAttach = 1
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object TLabel
            Caption = '_Unit Output Path'
            UseUnderline = True
            XAlign = 0
            TableRightAttach = 1
            TableTopAttach = 1
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object TLabel
            Caption = '_Search Path'
            UseUnderline = True
            XAlign = 0
            TableRightAttach = 1
            TableTopAttach = 2
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object TLabel
            Caption = '_Conditionals'
            XAlign = 0
            UseUnderline = True
            TableRightAttach = 1
            TableTopAttach = 4
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object THSeparator
            TableRightAttach = 2
            TableTopAttach = 3
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object entOutputPath: TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object entUnitOutputPath: TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableTopAttach = 1
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object entSearchPath: TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableTopAttach = 2
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object entConditionals: TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableTopAttach = 4
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
        end
      end
      object TNotebookPage
        Caption = 'Compiler'
      end
    end
    object THButtonBox
      Layout = bblEnd
      Spacing = 8
      BoxExpand = False
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
