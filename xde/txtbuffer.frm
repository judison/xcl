object TTxtBuffer
  object Buf: TSourceBuffer
    OnChanged = BufChanged
  end
  object TActionList
    object actEditUndo: TAction
      StockID = 'gtk-undo'
      OnExecute = EditUndo
      OnUpdate = EditUndoUpd
    end
    object actEditRedo: TAction
      StockID = 'gtk-redo'
      OnExecute = EditRedo
      OnUpdate = EditRedoUpd
    end
    object actEditCut: TAction
      StockID = 'gtk-cut'
      OnExecute = EditCut
      OnUpdate = EditCutUpd
    end
    object actEditCopy: TAction
      StockID = 'gtk-copy'
      OnExecute = EditCopy
      OnUpdate = EditCopyUpd
    end
    object actEditPaste: TAction
      StockID = 'gtk-paste'
      OnExecute = EditPaste
      OnUpdate = EditPasteUpd
    end
    object actEditClear: TAction
      StockID = 'gtk-clear'
      OnExecute = EditClear
      OnUpdate = EditClearUpd
    end
  end
  object TVBox
    object TToolBar
      ToolBarStyle = tbsIcons
      object TToolButton
        Action = actEditUndo
      end
      object TToolButton
        Action = actEditRedo
      end
      object TSeparatorToolItem
      end
      object TToolButton
        Action = actEditCut
      end
      object TToolButton
        Action = actEditCopy
      end
      object TToolButton
        Action = actEditPaste
      end
      object TToolButton
        Action = actEditClear
      end
      object TSeparatorToolItem
      end
    end
    object TScrolledWindow
      ShadowType = stIn
      object Edt: TSourceView
        FontDesc = 'Courier 10'
        TextBuffer = Buf
      end
    end
  end
end
