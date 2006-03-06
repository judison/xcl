(*
   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{$IFDEF INC_READ_INTERFACE}
//** INTERFACE
//****************************************************************************

  TListStore = class(TTreeModel)
  {$DEFINE INTF_DECLARATION}
    {$I itreesortable.inc}
  {$UNDEF INTF_DECLARATION}
  private
  protected
    procedure CreateHandle; override;
    procedure SetValue(const AIter: TTreeIter; ACol: Integer; AGValue: Pointer); override;
  public
    procedure Remove(const AIter: TTreeIter);
    procedure Insert(var AIter: TTreeIter; APosition: Integer);
    procedure InsertBefore(var AIter: TTreeIter; const ASibling: TTreeIter);
    procedure InsertAfter(var AIter: TTreeIter; const ASibling: TTreeIter);
    procedure Prepend(var AIter: TTreeIter);
    procedure Append(var AIter: TTreeIter);
    procedure Clear;
    procedure Swap(var A, B: TTreeIter);
    procedure MoveBefore(var AIter: TTreeIter; const APosition: TTreeIter);
    procedure MoveAfter(var AIter: TTreeIter; const APosition: TTreeIter);
  published
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TTreeStore }

procedure TListStore.CreateHandle;
var
  X: array of GType;
  I: Integer;
begin
  SetLength(X, Length(FStructure));
  for I := 1 to Length(FStructure) do
    case FStructure[I] of
      'I': X[I-1] := G_TYPE_INT;
      'U': X[I-1] := G_TYPE_UINT;
      'S': X[I-1] := G_TYPE_STRING;
      'B': X[I-1] := G_TYPE_BOOLEAN;
      'P': X[I-1] := G_TYPE_POINTER;
    else
      raise Exception.Create('Invalid Structure');
    end;
  Handle := gtk_list_store_newv(Length(FStructure), @X[0]);
  g_object_ref(Handle);
end;

procedure TListStore.SetValue(const AIter: TTreeIter; ACol: Integer; AGValue: Pointer);
begin
  gtk_list_store_set_value(Handle, @AIter, ACol, AGValue);
end;

procedure TListStore.Remove(const AIter: TTreeIter);
begin
  //Result := gtk_list_store_remove(Handle, @AIter);
  gtk_list_store_remove(Handle, @AIter);
end;

procedure TListStore.Insert(var AIter: TTreeIter; APosition: Integer);
begin
  gtk_list_store_insert(Handle, @AIter, APosition);
end;

procedure TListStore.InsertBefore(var AIter: TTreeIter; const ASibling: TTreeIter);
begin
  gtk_list_store_insert_before(Handle, @AIter, @ASibling);
end;

procedure TListStore.InsertAfter(var AIter: TTreeIter; const ASibling: TTreeIter);
begin
  gtk_list_store_insert_after(Handle, @AIter, @ASibling);
end;

procedure TListStore.Prepend(var AIter: TTreeIter);
begin
  gtk_list_store_prepend(Handle, @AIter);
end;

procedure TListStore.Append(var AIter: TTreeIter);
begin
  gtk_list_store_append(Handle, @AIter);
end;

procedure TListStore.Clear;
begin
  gtk_list_store_clear(Handle);
end;

procedure TListStore.Swap(var A, B: TTreeIter); // var ou const ??? GTK muda os Iter's ?
begin
  gtk_list_store_swap(Handle, @A, @B);
end;

procedure TListStore.MoveBefore(var AIter: TTreeIter; const APosition: TTreeIter);
begin
  gtk_list_store_move_before(Handle, @AIter, @APosition);
end;

procedure TListStore.MoveAfter(var AIter: TTreeIter; const APosition: TTreeIter);
begin
  gtk_list_store_move_after(Handle, @AIter, @APosition);
end;

{$DEFINE INTF_IMPLEMENTATION}
  {$define the_class := TListStore }
    {$I itreesortable.inc}
  {$undef the_class}
{$UNDEF INTF_IMPLEMENTATION}

{$ENDIF}
