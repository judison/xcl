object TMainForm
  Title = 'Pascal Editor'
  Width = 500
  Height = 300
  object TB: TPasTextBuffer
    OnChanged = TBChanged
  end
  object OpenDialog: TFileChooserDialog
    Title = 'Open...'
  end
  object SaveAsDialog: TFileChooserDialog
    FileAction = fcaSave
    Title = 'Save As...'
  end
  object TActionList
    object actFileNew: TAction
      StockID = 'gtk-new'
      OnExecute = FileNew
    end
    object actFileOpen: TAction
      StockID = 'gtk-open'
      OnExecute = FileOpen
    end
    object actFileSave: TAction
      StockID = 'gtk-save'
      OnExecute = FileSave
    end
    object actFileSaveAs: TAction
      StockID = 'gtk-save-as'
      OnExecute = FileSaveAs
    end
    object actFileQuit: TAction
      StockID = 'gtk-quit'
      OnExecute = FileQuit
    end
    object actEditUndo: TAction
      StockID = 'gtk-undo'
      OnExecute = EditUndo
    end
    object actEditRedo: TAction
      StockID = 'gtk-redo'
      OnExecute = EditRedo
    end
    object actEditCut: TAction
      StockID = 'gtk-cut'
      OnExecute = EditCut
    end
    object actEditCopy: TAction
      StockID = 'gtk-copy'
      OnExecute = EditCopy
    end
    object actEditPaste: TAction
      StockID = 'gtk-paste'
      OnExecute = EditPaste
    end
    object actEditDelete: TAction
      StockID = 'gtk-delete'
      OnExecute = EditDelete
    end
    object actCompileCompile: TAction
      Caption = '_Compile'
    end
    object actCompileRun: TAction
      Caption = '_Run'
      StockID = 'gtk-execute'
    end
    object actCompilePreferences: TAction
      StockID = 'gtk-preferences'
    end
    object actHelpAbout: TAction
      StockID = 'gtk-about'
    end
  end
  object TVBox
    object TMenuBar
      object TMenuItem
        Caption = '_File'
        object TMenuItem
          Action = actFileNew
        end
        object TMenuItem
          Action = actFileOpen
        end
        object TMenuItem
          Action = actFileSave
        end
        object TMenuItem
          Action = actFileSaveAs
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actFileQuit
        end
      end
      object TMenuItem
        Caption = '_Compile'
        object TMenuItem
          Action = actCompileCompile
        end
        object TMenuItem
          Action = actCompileRun
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actCompilePreferences
        end
      end
      object TMenuItem
        Caption = '_Edit'
        object TMenuItem
          Action = actEditUndo
        end
        object TMenuItem
          Action = actEditRedo
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actEditCut
        end
        object TMenuItem
          Action = actEditCopy
        end
        object TMenuItem
          Action = actEditPaste
        end
        object TMenuItem
          Action = actEditDelete
        end
      end
      object TMenuItem
        Caption = '_Help'
        RightJustified = True
        object TMenuItem
          Action = actHelpAbout
        end
      end
    end
    object TToolBar
      object TToolItem
        Action = actFileNew
      end
      object TToolItem
        Action = actFileOpen
      end
      object TToolItem
        Action = actFileSave
      end
      object TSeparatorToolItem
      end
      object TToolItem
        Action = actEditUndo
      end
      object TToolItem
        Action = actEditRedo
      end
      object TSeparatorToolItem
      end
      object TToolItem
        Action = actEditCut
      end
      object TToolItem
        Action = actEditCopy
      end
      object TToolItem
        Action = actEditPaste
      end
    end
    object TScrolledWindow
      ShadowType = stIn
      object TV: TTextView
        FontDesc = 'Courier 10'
        TextBuffer = TB
      end
    end
    object TStatusBar
      BoxExpand = False
    end
  end
end
