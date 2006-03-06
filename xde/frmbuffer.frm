object FrmBuffer: TFrmBuffer
  object ComponentTS: TTreeStore
    SortColumn = -2
    Structure = 'SP'
  end
  object THPaned
    object TVPaned
      object TVBox
        object TToolBar
          ToolBarStyle = tbsIcons
          object TToolItem
            Homogeneous = False
            object TButton
              Relief = rlfNone
              OnClicked = RemoveComp
              object TImage
                StockIconSize = iszMenu
                StockID = 'gtk-remove'
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
            Model = ComponentTS
            OnSelectionChanged = CompChanged
          end
        end
      end
      object TNotebook
        WidthRequest = 300
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
    object ClientBox: TVBox
      Spacing = 4
      object TScrolledWindow
        HPolicy = sbpAutomatic
        VPolicy = sbpAutomatic
        BoxExpand = True
        object ClientArea: TViewPort
        end
      end
    end
  end
end
