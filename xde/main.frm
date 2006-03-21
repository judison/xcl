object TMainForm
  Title = 'XDE'
  Width = 800
  Height = 600
  OnShow = MainFormShow
  object LangMan: TSourceLanguagesManager
  end
  object PBLogo: TPixbuf
    Resource = 'xde'
  end
  object FS: TFileChooserDialog
  end
  object AboutDlg: TAboutDialog
    AppName = 'XDE'
    Version = 'alpha'
    Copyright = 'Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>'
    Comments = 'XCL''s Development Environment'
    Website = 'http://xcl.sourceforge.net/'
    Logo = PBLogo
  end
  object FileBrowserTS: TTreeStore
    Structure = 'S'
  end
  object TActionList
    object actFileNew: TAction
      Accelerator = '<Control>n'
      Caption = '_New...'
      IconName = 'gtk-new'
      OnExecute = FileNew
    end
    object actFileOpen: TAction
      Accelerator = '<Control>o'
      Caption = '_Open'
      IconName = 'gtk-open'
      OnExecute = FileOpen
    end
    object actFileSave: TAction
      Accelerator = '<Control>s'
      Caption = '_Save'
      IconName = 'gtk-save'
      OnExecute = FileSave
      OnUpdate = FileSaveUpd
    end
    object actFileSaveAs: TAction
      Accelerator = '<Control><Shift>s'
      Caption = 'Save _as...'
      IconName = 'gtk-save-as'
      OnExecute = FileSaveAs
      OnUpdate = FileSaveAsUpd
    end
    object actFileClose: TAction
      Accelerator = '<Alt>q'
      Caption = '_Close'
      IconName = 'gtk-close'
      OnExecute = FileClose
      OnUpdate = FileCloseUpd
    end
    object actFileQuit: TAction
      Caption = '_Quit'
      IconName = 'gtk-quit'
      OnExecute = FileQuit
    end
    object actProjectAdd: TAction
      Caption = '_Add File'
      IconName = 'gtk-add'
    end
    object actProjectRemove: TAction
      Caption = '_Remove File'
      IconName = 'gtk-remove'
    end
    object actGoLeft: TAction
      Accelerator = '<Alt>Left'
      Caption = 'Go Left'
      OnExecute = GoLeft
    end
    object actGoRight: TAction
      Accelerator = '<Alt>Right'
      Caption = 'Go Right'
      OnExecute = GoRight
    end
    object actViewToggleFormCode: TAction
      Accelerator = 'F12'
      Caption = '_Toggle Form/Code'
      OnExecute = ToggleFormCode
    end
    object actViewObjectInspector: TAction
      Accelerator = 'F11'
      Caption = '_Object Inspector'
      OnExecute = ShowObjectInspector
    end
    object actViewProjectManager: TAction
      Accelerator = '<Control>F11'
      Caption = '_Project Manager'
      OnExecute = ShowProjectManager
    end
    object actViewFileBrowser: TAction
      Accelerator = '<Shift>F11'
      Caption = '_File Browser'
      OnExecute = ShowFileBrowser
    end
    object actHelpAbout: TAction
      Caption = '_About...'
      IconName = 'gtk-about'
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
        Caption = '_Project'
        object TMenuItem
          Caption = 'Add _New...'
          object TMenuItem
            Caption = 'Form'
          end
          object TMenuItem
            Caption = 'Unit'
          end
        end
        object TMenuItem
          Action = actProjectAdd
        end
        object TMenuItem
          Action = actProjectRemove
        end
      end
      object TMenuItem
        Caption = '_View'
        object TMenuItem
          Caption = 'Project Source'
        end
        object TMenuItem
          Action = actViewToggleFormCode
        end
        object TMenuItem
          Action = actViewObjectInspector
        end
        object TMenuItem
          Action = actViewProjectManager
        end
        object TMenuItem
          Action = actViewFileBrowser
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
    object THBox
      BoxExpand = False
      object TVBox
        BoxExpand = False
        object THBox
          object TToolItem
            BoxExpand = False
            Action = actFileNew
          end
          object TToolItem
            BoxExpand = False
            Action = actFileOpen
          end
          object TToolItem
            BoxExpand = False
            Action = actFileSave
          end
          object TToolItem
            BoxExpand = False
            Action = actFileSaveAs
          end
        end
        object THBox
          object TToolItem
            BoxExpand = False
            Action = actFileClose
          end
        end
      end
      object CompPalette: TComponentPalette
        BoxExpand = True
        OnClassSelected = PaletteClassSelected
      end
    end
    object THPaned
      object nbSide: TNotebook
        object npFileBrowser: TNotebookPage
          Caption = 'Browser'
          IconName = 'gtk-directory'
          object TScrolledWindow
            ShadowType = stIn
            HPolicy = sbpAutomatic
            VPolicy = sbpAutomatic
            HeightRequest = 200
            object FileBrowserTV: TTreeView
              Columns = <            
                item
                  Clickable = False
                  FixedWidth = 1
                  Sizing = tvcsAutosize
                  Title = 'File Tree'
                end>
              Model = FileBrowserTS
              HeadersVisible = False
              SelectionMode = smBrowse
              OnRowActivated = FileBrowserTVRowActivated
            end
          end
        end
        object npProjMan: TNotebookPage
          Caption = 'Project Manager'
          object TVBox
            object TToolBar
              ToolBarStyle = tbsIcons
              object TToolItem
                object TButton
                  Relief = rlfNone
                  object TImage
                    IconName = 'gtk-add'
                    IconSize = iszMenu
                  end
                end
              end
              object TToolItem
                object TButton
                  Relief = rlfNone
                  object TImage
                    IconName = 'gtk-remove'
                    IconSize = iszMenu
                  end
                end
              end
            end
            object TScrolledWindow
              ShadowType = stIn
              HPolicy = sbpAutomatic
              VPolicy = sbpAutomatic
              HeightRequest = 200
              BoxExpand = True
              object ProjectTV: TTreeView
                Columns = <            
                  item
                    Clickable = False
                    FixedWidth = 1
                    Sizing = tvcsAutosize
                    Title = 'Project Tree'
                  end>
                HeadersVisible = False
                SelectionMode = smBrowse
              end
            end
          end
        end
        object npObjIns: TNotebookPage
          Caption = 'Object Inspector'
          object TVPaned
            object TVBox
              object TToolBar
                ToolBarStyle = tbsIcons
                object TToolItem
                  Homogeneous = False
                  object TButton
                    Relief = rlfNone
                    object TImage
                      IconName = 'gtk-remove'
                      IconSize = iszMenu
                    end
                  end
                end
              end
              object TScrolledWindow
                ShadowType = stIn
                HPolicy = sbpAutomatic
                VPolicy = sbpAutomatic
                HeightRequest = 200
                BoxExpand = True
                object ComponentTV: TTreeView
                  Columns = <            
                    item
                      Clickable = False
                      FixedWidth = 1
                      Sizing = tvcsAutosize
                      Title = 'Component Tree'
                    end>
                  HeadersVisible = False
                  SelectionMode = smBrowse
                  OnSelectionChanged = CompChanged
                end
              end
            end
            object TNotebook
              object TNotebookPage
                Caption = 'Properties'
                object TScrolledWindow
                  HPolicy = sbpAutomatic
                  VPolicy = sbpAutomatic
                  object TViewPort
                    ShadowType = stNone
                    object PropTable: TTable
                      NCols = 1
                      NRows = 1
                    end
                  end
                end
              end
              object TNotebookPage
                Caption = 'Events'
                object TScrolledWindow
                  HPolicy = sbpAutomatic
                  VPolicy = sbpAutomatic
                  object TViewPort
                    ShadowType = stNone
                    object EventTable: TTable
                      NCols = 1
                      NRows = 1
                    end
                  end
                end
              end
            end
          end
        end
      end
      object NB: TNotebook
        Scrollable = True
        OnSwitchPage = SwitchPage
      end
    end
    object TStatusBar
      BoxExpand = False
    end
  end
end
