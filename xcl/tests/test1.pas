(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program test1;

{$H+}
{$Mode ObjFpc}

uses Classes, SysUtils, xcl, gtkspell, test1_xrc;

type
  TTest1 = class(TForm)
    Btn1, Btn2: TButton;
    Entry1: TEntry;
    Timer1: TTimer;
    Cal: TCalendar;
    TV: TTextView;
    CM: TListStore;
    Img: TImage;
    PB: TPixBuf;
    procedure MyFormShow(Sender: TObject);
    procedure Btn1FocusIn(Sender: TObject);
    procedure Btn1FocusOut(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CalChanged(Sender: TObject);
    procedure CloseFrm(Sender: TObject);
    procedure Entry1Changed(Sender: TObject);
    procedure OpenClicked(Sender: TObject);
  public
    procedure DoAha(var Msg: TXCLMsg); message 'aha';
    constructor Create(AOwner: TComponent); override;
  end;

constructor TTest1.Create(AOwner: TComponent);
var
  It: TTreeIter;
begin
  inherited;
  CM.Append(It); CM.SetStringValue(It, 0, 'Abacate');
  CM.Append(It); CM.SetStringValue(It, 0, 'Ameixa');
  CM.Append(It); CM.SetStringValue(It, 0, 'Banana');
  CM.Append(It); CM.SetStringValue(It, 0, 'Laranja');
  CM.Append(It); CM.SetStringValue(It, 0, 'Fruta do Conde');
  CM.Append(It); CM.SetStringValue(It, 0, 'Maca');
  CM.Append(It); CM.SetStringValue(It, 0, 'Melao');
  CM.Append(It); CM.SetStringValue(It, 0, 'Melancia');
  CM.Append(It); CM.SetStringValue(It, 0, 'Mixirica');
  CM.Append(It); CM.SetStringValue(It, 0, 'Pera');
  CM.Append(It); CM.SetStringValue(It, 0, 'Uva');
end;

procedure TTest1.OpenClicked(Sender: TObject);
var
  FCD: TFileChooserDialog;
begin
  FCD := TFileChooserDialog.Create(nil);
  FCD.Execute;
  PB.FileName := FCD.FileName;
  FCD.Free;
end;

procedure TTest1.Entry1Changed(Sender: TObject);
begin
  WriteLn('Entry1.Changed: ', Entry1.Text);
end;

procedure TTest1.DoAha(var Msg: TXCLMsg);
begin
  WriteLn('Aha, o sistema de mensagem do XCL funciona!!!!');
end;

procedure TTest1.CalChanged(Sender: TObject);
begin
  SendMessage('aha');
  WriteLn('Cal.Changed: ', DateToStr(Cal.Date));
end;

procedure TTest1.MyFormShow(Sender: TObject);
begin
  WriteLn('hehe OnShow');
end;

procedure TTest1.Btn1FocusIn(Sender: TObject);
begin
  WriteLn('Btn1 - Focus In');
end;

procedure TTest1.Btn1FocusOut(Sender: TObject);
begin
  WriteLn('Btn1 - Focus Out');
end;

procedure TTest1.Btn2Click(Sender: TObject);
var
  Stream: TStream;
  FCD: TFileChooserDialog;
begin
  //==
  WriteLn(TButton(Sender).Name + ' Clicked');
  Stream := TFileStream.Create('out.frm', fmCreate);
  WriteTxtFrm(Stream);
  Stream.Free;
  FCD := TFileChooserDialog.Create(nil);
//  FCD.Action := fcaSave;
  FCD.Execute;
  FCD.Free;
//  Close;
end;

procedure TTest1.Timer1Timer(Sender: TObject);
begin
  WriteLn('*** Timer!!');
end;

procedure TTest1.CloseFrm(Sender: TObject);
begin
  Close;
end;

var
  MyForm : TTest1;
begin
  Application.Initialize;
  Application.CreateForm(TTest1, MyForm);
  Application.Run;
end.
