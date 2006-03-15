object TTestTV
  Title = 'Teste TreeView'
  OnShow = MyFormShow
  object TS: TTreeStore
    Structure = 'SS'
  end
  object TActionList
    object actFileQuit: TAction
      Caption = '_Quit'
      IconName = 'gtk-quit'
      OnExecute = CloseFrm
    end
  end
  object TVBox
    object THandleBox
      BoxExpand = False
      object TMenuBar
        object TMenuItem
          Caption = '_File'
          object TMenuItem
            Action = actFileQuit
          end
        end
      end
    end
    object TScrolledWindow
      ShadowType = stIn
      object TV: TTreeView
        Columns = <
          item
            Title = 'Class'
            TextColumn = 0
          end
          item
            Title = 'Name'
            TextColumn = 1
          end
        >
        Model = TS
      end
    end
    object TStatusBar
      BoxExpand = False
    end
  end
end