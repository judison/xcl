object TTxtBuffer
  object Buf: TSourceBuffer
    OnChanged = BufChanged
  end
  object MainBox: TVBox
    object TextBox: TVBox
      object TScrolledWindow
        ShadowType = stIn
        object Edt: TSourceView
          FontDesc = 'Courier 10'
          TextBuffer = Buf
        end
      end
    end
  end
end
