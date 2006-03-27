object TMainForm
  Title = 'XDE'
  Width = 800
  Height = 600
  OnShow = MainFormShow
  object PBLogo: TPixbuf
    Resource = 'xde'
  end
  object fcdOpen: TFileChooserDialog
    FileAction = fcaOpen
    Title = 'Open File'
  end
  object fcdSaveAs: TFileChooserDialog
    FileAction = fcaSave
    Title = 'Save As...'
  end
  object fcdOpenProject: TFileChooserDialog
    FileAction = fcaOpen
    Title = 'Open Project'
  end
  object AboutDlg: TAboutDialog
    AppName = 'XDE'
    Version = 'alpha'
    Copyright = 'Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>'
    Comments = 'XCL''s Development Environment'
    Website = 'http://xcl.sourceforge.net/'
    Logo = PBLogo
  end
  object ProjectTS: TTreeStore
    Structure = 'SP'
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
      Caption = '_Open...'
      IconName = 'gtk-open'
      OnExecute = FileOpen
    end
    object actFileOpenProject: TAction
      Accelerator = '<Control>F11'
      Caption = 'Open Project...'
      IconName = 'gtk-open'
      OnExecute = FileOpenProject
    end
    object actFileSave: TAction
      Accelerator = '<Control>s'
      Caption = '_Save'
      IconName = 'gtk-save'
      OnExecute = FileSave
      OnUpdate = FileSaveUpd
    end
    object actFileSaveAs: TAction
      Caption = 'Save _As...'
      IconName = 'gtk-save-as'
      OnExecute = FileSaveAs
      OnUpdate = FileSaveAsUpd
    end
    object actFileSaveAll: TAction
      Caption = 'Sa_ve All'
      IconName = 'gtk-save'
      OnExecute = FileSaveAll
      OnUpdate = FileSaveAllUpd
    end
    object actFileClose: TAction
      Accelerator = '<Alt>q'
      Caption = '_Close'
      IconName = 'gtk-close'
      OnExecute = FileClose
      OnUpdate = FileCloseUpd
    end
    object actFileCloseAll: TAction
      Caption = 'C_lose All'
      IconName = 'gtk-close'
      OnExecute = FileCloseAll
      OnUpdate = FileCloseAllUpd
    end
    object actFileQuit: TAction
      Caption = '_Quit'
      IconName = 'gtk-quit'
      OnExecute = FileQuit
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
      Accelerator = '<Ctrl><Alt>F11'
      Caption = '_Project Manager'
      OnExecute = ShowProjectManager
    end
    object actProjectAdd: TAction
      Accelerator = '<Shift>F11'
      Caption = '_Add File...'
      IconName = 'gtk-add'
    end
    object actProjectRemove: TAction
      Caption = '_Remove File'
      IconName = 'gtk-remove'
    end
    object actProjectViewSource: TAction
      Caption = '_View Source'
    end
    object actProjectCompile: TAction
      Accelerator = '<Ctrl>F9'
      Caption = '_Compile'
      IconName = 'gtk-convert'
      OnExecute = ProjectCompile
    end
    object actProjectBuild: TAction
      Caption = '_Build'
      OnExecute = ProjectBuild
    end
    object actProjectRun: TAction
      Accelerator = 'F9'
      Caption = '_Run'
      IconName = 'gtk-execute'
      OnExecute = ProjectRun
    end
    object actProjectOptions: TAction
      Accelerator = '<Shift><Ctrl>F11'
      Caption = '_Options'
      IconName = 'gtk-preferences'
      OnExecute = ProjectOptions
    end
    object actRemoveComponent: TAction
      Caption = 'Remove Component'
      IconName = 'gtk-remove'
      OnExecute = RemoveComponent
    end
    object actAddComponentChild: TAction
      Caption = 'Add Component Child'
      IconName = 'gtk-add'
      OnExecute = AddComponentChild
      OnUpdate = AddComponentChildUpd
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
          Action = actFileOpenProject
        end
        object TMenuItem
          Action = actFileSave
        end
        object TMenuItem
          Action = actFileSaveAs
        end
        object TMenuItem
          Action = actFileSaveAll
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actFileClose
        end
        object TMenuItem
          Action = actFileCloseAll
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actFileQuit
        end
      end
      object TMenuItem
        Caption = '_View'
        object TMenuItem
          Action = actViewProjectManager
        end
        object TMenuItem
          Action = actViewObjectInspector
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actViewToggleFormCode
        end
      end
      object TMenuItem
        Caption = '_Project'
        object TMenuItem
          Action = actProjectAdd
        end
        object TMenuItem
          Action = actProjectRemove
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actProjectViewSource
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actProjectBuild
        end
        object TMenuItem
          Action = actProjectCompile
        end
        object TMenuItem
          Action = actProjectRun
        end
        object TSeparatorMenuItem
        end
        object TMenuItem
          Action = actProjectOptions
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
    object TopToolBox: THBox
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
            Action = actProjectCompile
          end
          object TToolItem
            BoxExpand = False
            Action = actProjectRun
          end
        end
      end
    end
    object THPaned
      object nbSide: TNotebook
        PanedShrink = False
        object npProjMan: TNotebookPage
          Caption = 'Project Manager'
          object TVBox
            object TToolBar
              ToolBarStyle = tbsIcons
              SmallIcons = True
              object TToolItem
                Action = actFileOpenProject
              end
              object TSeparatorToolItem
              end
              object TToolItem
                Action = actProjectAdd
              end
              object TToolItem
                Action = actProjectRemove
              end
              object TSeparatorToolItem
              end
              object TToolItem
                Action = actProjectOptions
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
                Model = ProjectTS
                OnRowActivated = ProjectTVRowActivated
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
                SmallIcons = True
                object TToolItem
                  Action = actAddComponentChild
                end
                object TToolItem
                  Action = actRemoveComponent
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
