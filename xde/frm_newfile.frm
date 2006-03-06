object FrmNewFile: TFrmNewFile
  Title = 'New File'
  BorderWidth = 8
  object TVBox
    Spacing = 8
    object TLabel
      Caption = 'Chose what kind of file you want.'
    end
    object THSeparator
    end
    object THButtonBox
      Layout = bblSpread
      object TButton
        Caption = 'Unit'
        OnClicked = BtnClicked
        Tag = 1
      end
      object TButton
        Caption = 'Form'
        OnClicked = BtnClicked
        Tag = 2
      end
    end
  end
end
