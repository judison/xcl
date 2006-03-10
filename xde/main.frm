object TMainForm
  Title = 'XDE: XCL''s Development Environment'
  Width = 800
  Height = 600
  object LangMan: TSourceLanguagesManager
  end
  object FS: TFileChooserDialog
  end
  object AboutDlg: TAboutDialog
    AppName = 'XDE'
    Version = 'alpha'
    Copyright = 'Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>'
    Comments = 'XCL''s Development Environment'
    Website = 'http://xcl.sourceforge.net/'
  end
  object TAccelerator
    AccelName = '<Alt>Left'
    OnActivate = GoLeft
  end
  object TAccelerator
    AccelName = '<Alt>Right'
    OnActivate = GoRight
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
      OnUpdate = FileSaveUpd
    end
    object actFileSaveAs: TAction
      StockID = 'gtk-save-as'
      OnExecute = FileSaveAs
      OnUpdate = FileSaveAsUpd
    end
    object actFileClose: TAction
      StockID = 'gtk-close'
      OnExecute = FileClose
      OnUpdate = FileCloseUpd
    end
    object actFileQuit: TAction
      StockID = 'gtk-quit'
      OnExecute = FileQuit
    end
    object actHelpAbout: TAction
      StockID = 'gtk-about'
      OnExecute = HelpAbout
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
          Action = actFileClose
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actFileQuit
        end
      end
      object TMenuItem
        Caption = '_Options'
        object TMenuItem
          Caption = 'Compiler Options'
          OnClick = ShowCompilerOptions
        end
        object TMenuItem
          Caption = 'Editor Options'
          OnClick = ShowEditorOptions
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
      ToolBarStyle = tbsIcons
      object TToolButton
        Action = actFileNew
      end
      object TToolButton
        Action = actFileOpen
      end
      object TToolButton
        Action = actFileSave
      end
      object TSeparatorToolItem
      end
      object TToolButton
        Action = actFileClose
      end
      object TSeparatorToolItem
      end
    end
    object NB: TNotebook
    end
    object TStatusBar
      BoxExpand = False
    end
  end
end
