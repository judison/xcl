object FrmEditFind: TFrmEditFind
  Title = 'Find'
  BorderWidth = 3
  object TVBox
    Spacing = 4
    object THBox
      Spacing = 8
      BoxExpand = True
      object TLabel
        Caption = 'Text to Find'
        BoxExpand = True
      end
      object entText: TEntry
        BoxExpand = True
      end
    end
    object THSeparator
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
        Caption = '_Find'
        IconName = 'gtk-find'
        OnClicked = BtnFindClicked
        BoxExpand = True
      end
    end
  end
end
