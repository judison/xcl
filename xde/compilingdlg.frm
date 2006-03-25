object FrmCompilingDlg: TFrmCompilingDlg
  Title = 'Compiling...'
  BorderWidth = 3
  Resizable = False
  object TVBox
    Spacing = 4
    object TFrame
      ShadowType = stOut
      BoxExpand = True
      object TVBox
        Spacing = 4
        BorderWidth = 4
        object TFrame
          ShadowType = stIn
          BoxExpand = True
          object lblProject: TLabel
            Caption = '<b>Project:</b>'
            UseMarkup = True
            XAlign = 0
          end
        end
        object TFrame
          ShadowType = stIn
          BoxExpand = True
          object lblStatus: TLabel
            Caption = 'Calling compiler...'
            XAlign = 0
          end
        end
        object THBox
          Spacing = 4
          BoxExpand = True
          object TFrame
            ShadowType = stIn
            BoxExpand = True
            object lblHints: TLabel
              Caption = '<b>Hints:</b> 0'
              UseMarkup = True
              XAlign = 0
            end
          end
          object TFrame
            ShadowType = stIn
            BoxExpand = True
            object lblWarnings: TLabel
              Caption = '<b>Warnings:</b> 0'
              UseMarkup = True
              XAlign = 0
            end
          end
          object TFrame
            ShadowType = stIn
            BoxExpand = True
            object lblErrors: TLabel
              Caption = '<b>Errors:</b> 0'
              UseMarkup = True
              XAlign = 0
            end
          end
        end
      end
    end
    object THSeparator
    end
    object THButtonBox
      object Btn: TButton
        Caption = '_OK'
        IconName = 'gtk-ok'
        OnClicked = BtnOKClicked
        BoxExpand = True
      end
    end
  end
end
