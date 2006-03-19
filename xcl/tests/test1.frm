object TTest1
  Title = 'Teste Ola Mundo'
  OnShow = MyFormShow
  object PB: TPixBuf
    IconName = 'stock_bell'
    IconSize = 32
  end
  object TActionList
    object actFileNew: TAction
      Caption = '_New'
      IconName = 'gtk-new'
    end
    object actFileOpen: TAction
      Caption = '_Open'
      IconName = 'gtk-open'
      OnExecute = OpenClicked
    end
    object actFileSave: TAction
      Caption = '_Save'
      IconName = 'gtk-save'
    end
    object actFileSaveAs: TAction
      Caption = 'Save _as...'
      IconName = 'gtk-save-as'
    end
    object actFileQuit: TAction
      Caption = '_Quit'
      IconName = 'gtk-quit'
      OnExecute = CloseFrm
    end
  end
  object CM: TListStore
  end
  object TVBox
    object THandleBox
      BoxExpand = False
      object TMenuBar
        object TMenuItem
          Caption = '_File'
          object TMenuItem
            action = actFileNew
          end
          object TMenuItem
            action = actFileOpen
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
          Caption = 'Ajuda'
          RightJustified = True
          object TMenuItem
            Caption = 'Sobre...'
          end
        end
      end
    end
    object THPaned
      object TVBox
        object THBox
          BoxExpand = False
          Spacing = 4
          BorderWidth = 4
          object Cal: TCalendar
            OnChange = CalChanged
          end
          object TVBox
            Spacing = 4
            object TLabel
              Caption = '_Label, com 2 Linhas'#10'Alinhadas aki'
              XAlign = 1
              FocusControl = Entry1
              Justify = jsRight
              UseUnderline = True
            end
            object Entry1: TEntry
              Text = 'Edit-me'
              OnChanged = Entry1Changed
            end
            object TCheckButton
              Caption = 'Sim ou Nao?'
            end
            object THBox
              object TFontButton
              end
              object TColorButton
              end
            end
            object TComboBox
              Model = CM
            end
            object TComboBoxEntry
              Model = CM
            end
          end
          object Btn1: TButton
            Caption = 'Click-Me'
            OnClicked = Btn2Click
            OnFocusIn = Btn1FocusIn
            OnFocusOut = Btn1FocusOut
          end
          object TVScrollBar
          end
          object THScale
          end
          object TFrame
            Caption = 'My Frame'
            object TVBox
              BorderWidth = 4
              Spacing = 4
              object Btn2: TButton
                OnClicked = Btn2Click
                Relief = rlfNone
                object TImage
                  StockID = 'gtk-dialog-info'
                  IconSize = iszDialog
                end
              end
            end
          end
        end
        object THSeparator
        end
        object THBox
          BoxExpand = False
          Spacing = 4
          BorderWidth = 4
          object TArrow
          end
          object TProgressBar
          end
        end
        object THSeparator
        end
        object TNotebook
          object TNotebookPage
            Caption = 'TextView'
            object TVBox
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
                object TToolButton
                  StockId = 'gtk-undo'
                end
                object TToolButton
                  StockId = 'gtk-redo'
                end
                object TToolButton
                  StockId = 'gtk-cut'
                end
                object TToolButton
                  StockId = 'gtk-copy'
                end
                object TToolButton
                  StockId = 'gtk-paste'
                end
                object TSeparatorToolItem
                end
                object TToolItem
                  object TEntry
                    Text = 'URL'
                  end
                end
                object TSeparatorToolItem
                end
                object TToolItem
                  Action = actFileQuit
                end
              end
              object TScrolledWindow
                ShadowType = stIn
                object TV: TTextView
                  TextBuffer.Text = 'Hello world.'
                end
              end
            end
          end
          object TNotebookPage
            Caption = 'TColorSelection'
            object TColorSelection
            end
          end
          object TNotebookPage
            Caption = 'TImage'
            object Img: TImage
              PixBuf = PB
            end
          end
        end
        object THSeparator
        end
        object THButtonBox
          BorderWidth = 4
          BoxExpand = False
          object TButton
            Caption = '_OK'
            IconName = 'gtk-ok'
            ToolTip = 'Botao OK Hhduahua'
          end
          object TButton
            Caption = '_Cancel'
            IconName = 'gtk-cancel'
          end
          object TButton
            Caption = '_Help'
            IconName = 'gtk-help'
          end
        end
      end
    end
    object TStatusBar
      BoxExpand = False
    end
  end
  object Timer1: TTimer
    Active = False
    Interval = 500
    OnTimer = Timer1Timer
  end
  object TGtkSpell
    TextView = TV
  end
end