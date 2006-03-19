(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit main;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, Buffer, xclsourceview, componentpalette,
  propeditor, designform, uproject;

type
  TMainForm = class(TForm)
    LangMan: TSourceLanguagesManager;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileClose: TAction;
    actFileQuit: TAction;
    AboutDlg: TAboutDialog;
    MenuBar: TMenuBar;
    PBLogo: TPixbuf;
    NB: TNoteBook;
    FS: TFileChooserDialog;
    CompPalette: TComponentPalette;
    ProjectTV: TTreeView;
    ComponentTV: TTreeView;
    FileBrowserTV: TTreeView;
    FileBrowserTS: TTreeStore;
    PropTable: TTable;
    EventTable: TTable;
    nbSide: TNotebook;
    npFileBrowser: TNotebookPage;
    npProjMan: TNotebookPage;
    npObjIns: TNotebookPage;
    procedure FileOpen(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveUpd(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FileSaveAsUpd(Sender: TObject);
    procedure FileClose(Sender: TObject);
    procedure FileCloseUpd(Sender: TObject);
    procedure FileQuit(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure ShowCompilerOptions(Sender: TObject);
    procedure ShowEditorOptions(Sender: TObject);
    procedure GoLeft(Sender: TObject);
    procedure GoRight(Sender: TObject);
    procedure ToggleFormCode(Sender: TObject);
    procedure ShowObjectInspector(Sender: TObject);
    procedure CompChanged(Sender: TObject);
    procedure PaletteClassSelected(Sender: TObject; AClass: TComponentClass);
    procedure RemoveComp(Sender: TObject);
    procedure SwitchPage(Sender: TObject; NewPage: Integer);
    procedure FileBrowserTVRowActivated(Sender: TObject; const Iter: TTreeIter; Column: TTreeViewColumn);
    procedure MainFormShow(Sender: TObject);
  private
    function CurrentBuffer: TBuffer;
    function AddTree(C: TComponent; P: TTreeIter): TTreeIter;
    //--
    procedure UpdateFileBrowser;
  protected
    procedure DoCloseQuery(CanClose: Boolean); override;
  public
    CompEd: TComponentEditor;
    MyForm: TDesignForm;
    Project: TProject;
    //--
    constructor Create(AOwner: TComponent); override;
    //--
    procedure SelectComp(C: TComponent);
    procedure SelectForm(B: TBuffer);
    //--
    procedure DoFileOpen(AFileName: String);
  end;

var
  MainForm: TMainForm;

implementation

uses compiler_opts, editor_opts, TxtBuffer, PasBuffer, FrmBuffer, frm_NewFile;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Project := TProject.Create;
  UpdateFileBrowser;
end;

function TMainForm.CurrentBuffer: TBuffer;
begin
  Result := TBuffer(NB.Pages[NB.CurrentPage]);
end;

procedure TMainForm.FileNew(Sender: TObject);
var
  B: TBuffer;
begin
  B := nil;
  frmNewFile := TFrmNewFile.Create(nil);
  try
    frmNewFile.Tag := 0;
    frmNewFile.ShowModal;
    case frmNewFile.Tag of
      1: B := TPasBuffer.Create(Self);
      2: B := TFrmBuffer.Create(Self);
    end;
  finally
    frmNewFile.Free;
  end;
  if Assigned(B) then
  begin
    B.Parent := NB;
    B.New;
  end;
end;

procedure TMainForm.FileOpen(Sender: TObject);
begin
  FS.FileAction := fcaOpen;
  FS.Title := 'Open...';
  if FS.Execute = -3 then
    DoFileOpen(FS.FileName);
end;

procedure TMainForm.DoFileOpen(AFileName: String);
var
  B: TBuffer;
  ext: String;
begin
  if FileExists(ChangeFileExt(AFileName, '.frm')) then
    AFileName := ChangeFileExt(AFileName, '.frm');

  ext := LowerCase(ExtractFileExt(AFileName));

  if (ext = '.pas') or (ext = '.pp') or (ext = '.inc') or (ext = '.dpr') or (ext = '.p') then
    B := TPasBuffer.Create(Self)
  else if (ext = '.frm') then
    B := TFrmBuffer.Create(Self)
  else
    B := TTxtBuffer.Create(Self);
  B.Parent := NB;
  B.Open(AFileName);
  NB.CurrentPage := NB.PageNum(B);
  if not Assigned(MyForm) then
    SelectForm(B); // this only on first opened file, others will "SwitchPage"
end;

procedure TMainForm.MainFormShow(Sender: TObject);
begin
  Icon := PBLogo;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  CurrentBuffer.Save;
end;

procedure TMainForm.FileSaveUpd(Sender: TObject);
begin
  actFileSave.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  FS.FileAction := fcaSave;
  FS.Title := 'Save As...';
  if FS.Execute = -3 then
    CurrentBuffer.SaveAs(FS.FileName);
end;

procedure TMainForm.FileSaveAsUpd(Sender: TObject);
begin
  actFileSaveAs.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileClose(Sender: TObject);
begin
  CurrentBuffer.Free;
end;

procedure TMainForm.FileCloseUpd(Sender: TObject);
begin
  actFileClose.Sensitive := CurrentBuffer <> nil;
end;


procedure TMainForm.DoCloseQuery(CanClose: Boolean);
var
  Buffer: TBuffer;
  I: Integer;
begin
  inherited;
  for I := 0 to NB.PageCount -1 do
  begin
    Buffer := TBuffer(NB.Pages[I]);
    if Buffer.Modified then
      Abort;
      //CanClose := False;
  end;
end;

procedure TMainForm.FileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HelpAbout(Sender: TObject);
var
  SL: TStringList;
  S: TStream;
begin
  try
    S := TResourceStream.Create('STR', 'license');
    try
      SL := TStringList.Create;
      SL.LoadFromStream(S);
      //AboutDlg.WrapLicense := True;
      AboutDlg.License := SL.Text;
      SL.Free;
    finally
      S.Free;
    end;
  except
    // Does not show License Button on About Dialog :D
  end;
  AboutDlg.Execute;
end;

procedure TMainForm.ShowCompilerOptions(Sender: TObject);
begin
  FrmCompilerOpts := TFrmCompilerOpts.Create(nil);
  try
    FrmCompilerOpts.ShowModal;
  finally
    FrmCompilerOpts.Free;
  end;
end;

procedure TMainForm.ShowEditorOptions(Sender: TObject);
begin
  FrmEditorOpts := TFrmEditorOpts.Create(nil);
  try
    FrmEditorOpts.ShowModal;
  finally
    FrmEditorOpts.Free;
  end;
end;

procedure TMainForm.SwitchPage(Sender: TObject; NewPage: Integer);
begin
  SelectForm(TBuffer(NB.Pages[NewPage]));
end;

procedure TMainForm.GoLeft(Sender: TObject);
begin
  NB.PrevPage;
end;

procedure TMainForm.GoRight(Sender: TObject);
begin
  NB.NextPage;
end;

procedure TMainForm.UpdateFileBrowser;
var
  Info : TSearchRec;
  Ext: String;
  It: TTreeIter;
  DSL: TStringList;
  FSL: TStringList;
  I: Integer;
begin
  FileBrowserTS.Clear;

  DSL := TStringList.Create;
  FSL := TStringList.Create;
  try
  
    if FindFirst ('*', faAnyFile, Info) = 0 then
    repeat
      Ext := LowerCase(ExtractFileExt(Info.Name));
      if (Ext = '.pas') or (Ext = '.pp') or (Ext = '.inc') or (Ext = '.frm') then
        FSL.Add(Info.Name)
      else if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') then
        DSL.Add(Info.Name + '/');
    until FindNext(Info) <> 0;
    FindClose(Info);

    DSL.Sort;
    FSL.Sort;

    for I := 0 to DSL.Count -1 do
    begin
      FileBrowserTS.Append(It);
      FileBrowserTS.SetStringValue(It, 0, DSL[I]);
    end;

    for I := 0 to FSL.Count -1 do
    begin
      FileBrowserTS.Append(It);
      FileBrowserTS.SetStringValue(It, 0, FSL[I]);
    end;

  finally
    DSL.Free;
    FSL.Free;
  end;
end;

procedure TMainForm.FileBrowserTVRowActivated(Sender: TObject; const Iter: TTreeIter; Column: TTreeViewColumn);
var
  FN: String;
begin
  FN := FileBrowserTS.GetStringValue(Iter, 0);
  if FN[Length(FN)] = '/' then
  begin
    ChDir(FN);
    UpdateFileBrowser;
  end
  else
  begin
    DoFileOpen(FN);
  end;
end;

procedure TMainForm.CompChanged(Sender: TObject);
var
  It: TTreeIter;
  C: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTV.Model.GetPointerValue(It, 1));

    if not Assigned(CompEd) then
    begin
      CompEd := TComponentEditor.Create(Self);
      CompEd.PropTable := PropTable;
      CompEd.EventTable := EventTable;
    end;

    CompEd.Component := C;
  end;
end;

function TMainForm.AddTree(C: TComponent; P: TTreeIter): TTreeIter;
var
  It: TTreeIter;
  I: Integer;
  S: String;
begin
  if C.Name <> '' then
    S := C.Name + ': ' + C.ClassName
  else
    S := '['+C.ClassName+']';

  TTreeStore(ComponentTV.Model).Append(It, P);
  ComponentTV.Model.SetStringValue(It, 0, S);
  ComponentTV.Model.SetPointerValue(It, 1, C);

  if C is TControl then
    for I := 0 to TControl(C).ControlCount -1 do
      AddTree(TControl(C).Controls[I], It);

  Result := It;
end;

procedure TMainForm.PaletteClassSelected(Sender: TObject; AClass: TComponentClass);
var
  C: TComponent;
  It: TTreeIter;
begin
  if Assigned(CompEd) and Assigned(CompEd.Component) then
  begin
    C := AClass.Create(MyForm);
    try
      if (C is TControl) then
        if (CompEd.Component is TContainerControl) then
        begin
          TControl(C).Parent := TControl(CompEd.Component);
          ComponentTV.GetSelected(It);
        end
        else
          raise Exception.Create('A Control can only be place inside a Container Contol')
      else
        ComponentTV.Model.GetIterFirst(It);
    except
      C.Free;
      raise;
    end;

    It := AddTree(C, It);
    ComponentTV.ExpandTo(It);
    ComponentTV.SelectIter(It);
  end;
end;

procedure TMainForm.SelectComp(C: TComponent);

  function FindIter(var A: TTreeIter): boolean;
  var
    B: TTreeIter;
  begin
    Result := False;
    if TComponent(ComponentTV.Model.GetPointerValue(A, 1)) = C then
      exit(True);
    if ComponentTV.Model.IterChildren(B, A) then
      if FindIter(B) then
      begin
        A := B;
        exit(True);
      end;
    if ComponentTV.Model.IterNext(A) then
      Result := FindIter(A);
  end;

var
  It: TTreeIter;
begin
  if Assigned(C) and Assigned(ComponentTV.Model) and ComponentTV.Model.GetIterFirst(It) and FindIter(It) then
  begin
    ComponentTV.ExpandTo(It);
    ComponentTV.SelectIter(It);

    if not Assigned(CompEd) then
    begin
      CompEd := TComponentEditor.Create(Self);
      CompEd.PropTable := PropTable;
      CompEd.EventTable := EventTable;
    end;

    CompEd.Component := C;
  end;
end;

procedure TMainForm.RemoveComp(Sender: TObject);
var
  It: TTreeIter;
  C,P: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTV.Model.GetPointerValue(It, 1));
    if C <> MyForm then
    begin
      if C is TControl then
      begin
        P := TControl(C).Parent;
        TControl(C).Parent := nil;
      end
      else
        P := MyForm;
  
      TTreeStore(ComponentTV.Model).Remove(It);
      SelectComp(P);
      C.Free;
    end;
  end;
end;

procedure TMainForm.SelectForm(B: TBuffer);
var
  FB: TFrmBuffer;
begin
  if B is TFrmBuffer then
  begin
    FB := TFrmBuffer(B);
    MyForm := FB.MyForm;
    ComponentTV.Model := FB.ComponentTS;
  end
  else
  begin
    MyForm := nil;
    ComponentTV.Model := nil;
    CompED.Component := nil;
  end;
end;

procedure TMainForm.ToggleFormCode(Sender: TObject);
var
  FB: TFrmBuffer;
begin
  if CurrentBuffer is TFrmBuffer then
  begin
    FB := TFrmBuffer(CurrentBuffer);
    if FB.TextBox.Visible then
    begin
      FB.TextBox.Visible := False;
      FB.FormBox.Visible := True;
    end
    else
    begin
      FB.FormBox.Visible := False;
      FB.TextBox.Visible := True;
      FB.Edt.GrabFocus;
    end;
  end;
end;

procedure TMainForm.ShowObjectInspector(Sender: TObject);
begin
  nbSide.CurrentPage := nbSide.PageNum(npObjIns);
end;

end.
