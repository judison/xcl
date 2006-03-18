object FrmEditorOpts: TFrmEditorOpts
  Title = 'Editor Options'
  BorderWidth = 4
  object TVBox
    Spacing = 4
    object TNotebook
      BoxExpand = True
      object TNotebookPage
        Caption = 'General'
        object TFrame
        end
      end
      object TNotebookPage
        Caption = 'Syntax Highlight'
        object TFrame
        end
      end
    end
    object THSeparator
    end
    object THButtonBox
      Layout = bblEnd
      BoxExpand = False
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
