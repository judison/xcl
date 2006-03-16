object TTxtBuffer
  object Buf: TSourceBuffer
    OnChanged = BufChanged
  end
  object TActionList
    object actEditUndo: TAction
      Caption = '_Undo'
      IconName = 'gtk-undo'
      OnExecute = EditUndo
      OnUpdate = EditUndoUpd
    end
    object actEditRedo: TAction
      Caption = '_Redo'
      IconName = 'gtk-redo'
      OnExecute = EditRedo
      OnUpdate = EditRedoUpd
    end
    object actEditCut: TAction
      Caption = 'C_ut'
      IconName = 'gtk-cut'
      OnExecute = EditCut
      OnUpdate = EditCutUpd
    end
    object actEditCopy: TAction
      Caption = '_Copy'
      IconName = 'gtk-copy'
      OnExecute = EditCopy
      OnUpdate = EditCopyUpd
    end
    object actEditPaste: TAction
      Caption = '_Paste'
      IconName = 'gtk-paste'
      OnExecute = EditPaste
      OnUpdate = EditPasteUpd
    end
    object actEditDelete: TAction
      Caption = '_Delete'
      IconName = 'gtk-delete'
      OnExecute = EditDelete
      OnUpdate = EditDeleteUpd
    end
  end
  object MainBox: TVBox
    object TextBox: TVBox
      object TToolBar
        ToolBarStyle = tbsIcons
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
        object TToolItem
          Action = actEditDelete
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
end
