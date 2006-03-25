object FrmProjectOpts: TFrmProjectOpts
  Title = 'Project Options'
  BorderWidth = 3
  object TVBox
    Spacing = 4
    object TNotebook
      BoxExpand = True
      object TNotebookPage
        Caption = 'Main'
      end
      object TNotebookPage
        Caption = 'Directories/Conditionals'
        object TTable
          NCols = 3
          NRows = 5
          BorderWidth = 2
          object TLabel
            Caption = '_Output Directory'
            UseUnderline = True
            TableRightAttach = 1
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object TLabel
            Caption = '_Unit Output Directory'
            UseUnderline = True
            TableRightAttach = 1
            TableTopAttach = 1
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object TLabel
            Caption = '_Search Path'
            UseUnderline = True
            TableRightAttach = 1
            TableTopAttach = 2
            TableXPadding = 2
            TableXOptions = [aoFill]
            TableYOptions = [aoFill]
          end
          object TLabel
            Caption = '_Conditionals'
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
          object TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableTopAttach = 1
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object TEntry
            TableBottomAttach = 1
            TableLeftAttach = 1
            TableRightAttach = 2
            TableTopAttach = 2
            TableXPadding = 2
            TableYPadding = 2
            TableXOptions = [aoExpand, aoFill]
            TableYOptions = [aoExpand, aoFill]
          end
          object TEntry
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
