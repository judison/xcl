inherited FrmBuffer: TFrmBuffer
  object ComponentTS: TTreeStore
    SortColumn = -2
    Structure = 'SP'
  end
  inherited MainBox: TVBox
    object FormBox: TVBox
      object TScrolledWindow
        HPolicy = sbpAutomatic
        VPolicy = sbpAutomatic
        object ClientArea: TViewPort
        end
      end
    end
  end
end
