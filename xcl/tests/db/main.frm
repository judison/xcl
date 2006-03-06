object TMainForm
  Title = 'DB-Aware Components Test'
  object Table: TSdfDataSet
    Delimiter = ';'
    FileName = 'data.sdf'
    FirstLineAsSchema = True
  end
  object dsMain: TDataSource
    DataSet = Table
  end
  object TActionList
    object actFileQuit: TAction
      StockID = 'gtk-quit'
      OnExecute = FileQuit
    end
    object actDBFirst: TDataSetFirst
      DataSource = dsMain
    end
    object actDBPrior: TDataSetPrior
      DataSource = dsMain
    end
    object actDBNext: TDataSetNext
      DataSource = dsMain
    end
    object actDBLast: TDataSetLast
      DataSource = dsMain
    end
    object actDBInsert: TDataSetInsert
      DataSource = dsMain
    end
    object actDBEdit: TDataSetEdit
      DataSource = dsMain
    end
    object actDBDelete: TDataSetDelete
      DataSource = dsMain
    end
    object actDBPost: TDataSetPost
      DataSource = dsMain
    end
    object actDBCancel: TDataSetCancel
      DataSource = dsMain
    end
  end
  object TVBox
    object TMenuBar
      object TMenuItem
        Caption = '_File'
        object TMenuItem
          Action = actFileQuit
        end
      end
    end
    object TToolBar
      object TToolItem
        Action = actDBFirst
      end
      object TToolItem
        Action = actDBPrior
      end
      object TToolItem
        Action = actDBNext
      end
      object TToolItem
        Action = actDBLast
      end
      object TSeparatorToolItem
      end
      object TToolItem
        Action = actDBInsert
      end
      object TToolItem
        Action = actDBEdit
      end
      object TSeparatorToolItem
      end
      object TToolItem
        Action = actDBDelete
      end
      object TSeparatorToolItem
      end
      object TToolItem
        Action = actDBPost
      end
      object TToolItem
        Action = actDBCancel
      end
    end
    object THBox
      object TVBox
        object TLabel
          Caption = 'Name'
        end
        object TDBEntry
          DataSource = dsMain
          DataField = 'NAME'
        end
        object TDBLabel
          DataSource = dsMain
          DataField = 'NAME'
        end
      end
      object TVBox
        object TLabel
          Caption = 'Age'
        end
        object TDBEntry
          DataSource = dsMain
          DataField = 'AGE'
        end
      end
    end
    object TStatusBar
      BoxExpand = False
    end
  end
end
