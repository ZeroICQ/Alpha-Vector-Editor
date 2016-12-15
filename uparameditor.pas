unit UParamEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, UTransform, StdCtrls,
  ExtCtrls, Spin, FPCanvas, UParameters;

type

  { TParamEditor }

  TParamEditor = class
  private
    FLabel: TLabel;
    FComponent: TControl;
    FParams: TParamArr;
    FGControl: TGraphicControl;
  public
    procedure AttachParams(AParams: TParamArr; AGCotnrol: TGraphicControl); virtual;
    procedure SetParam(AParam: TParam); virtual; abstract;
    function GetParamType: TParamClass; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLineWidthParamEditor }

  TLineWidthParamEditor = class(TParamEditor)
  public
    procedure AttachParams(AParams: TParamArr; AGCotnrol: TGraphicControl); override;
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    procedure OnLineWidthChange(Sender: TObject);
    constructor Create; override;
  end;

  { TLineStyleParamEditor }

  TLineStyleParamEditor = class(TParamEditor)
  private
    procedure OnDrawLineStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    procedure AttachParams(AParams: TParamArr; AGCotnrol: TGraphicControl); override;
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    procedure OnLineStyleChange(Sender: TObject);
    constructor Create; override;
  end;

  { TBrushStyleParamEditor }

  TBrushStyleParamEditor = class(TParamEditor)
  private
    procedure OnDrawBrushStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    procedure AttachParams(AParams: TParamArr; AGCotnrol: TGraphicControl); override;
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    procedure OnBrushStyleChange(Sender: TObject);
    constructor Create; override;
  end;

  { TCornersParamEdtitor }

  TCornersParamEdtitor = class(TParamEditor)
  public
    procedure AttachParams(AParams: TParamArr; AGCotnrol: TGraphicControl); override;
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    procedure OnCornersNumberChange(Sender: TObject);
    constructor Create; override;
  end;

  { TIntegerParamEditor }

  TIntegerParamEditor = class(TParamEditor)
  public
    procedure AttachParams(AParams: TParamArr; AGCotnrol: TGraphicControl); override;
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    procedure OnIntegerChange(Sender: TObject);
    constructor Create(ACaption: String; AValue: Integer = 1);
  end;

  { TXCoeffParamEditor }

  TXCoeffParamEditor = class(TIntegerParamEditor)
  public
    function GetParamType: TParamClass; override;
    constructor Create(AValue: Integer = 1);
  end;

  { TYCoeffParamEditor }

  TYCoeffParamEditor = class(TIntegerParamEditor)
    function GetParamType: TParamClass; override;
    constructor Create(AValue: Integer = 1);
  end;

  TParamEditorClass = class of TParamEditor;
  TParamEditorsClassArr = array of TParamEditorClass;

  procedure ShowParamEditors(AEditors: array of TParamEditor; APanel: TPanel);
  function GetAllEditors: TParamEditorsClassArr;

implementation

uses
  LCLType;

{ Misc }

procedure Push(var AArr: TParamEditorsClassArr; AElement: TParamEditorClass);
begin
  SetLength(AArr, Length(AArr) + 1);
  AArr[High(AArr)] := AElement;
end;

procedure ShowParamEditors(AEditors: array of TParamEditor; APanel: TPanel);
var i: Integer;
begin
  for i := Low(AEditors) to High(AEditors) do begin
    with AEditors[i] do begin
      FLabel.Top := i * 50;
      FLabel.Left := 2;
      FLabel.Parent := APanel;
      FComponent.Top := i * 50 + FLabel.ClientHeight + 5;
      FComponent.Left := APanel.ClientWidth - FComponent.ClientWidth;
      FComponent.Parent := APanel;
    end;
  end;
end;

function GetAllEditors: TParamEditorsClassArr;
begin
  Push(Result, TLineWidthParamEditor);
  Push(Result, TLineStyleParamEditor);
  Push(Result, TBrushStyleParamEditor);
  Push(Result, TCornersParamEdtitor);
  Push(Result, TIntegerParamEditor);
end;

{ TYCoeffParamEditor }

function TYCoeffParamEditor.GetParamType: TParamClass;
begin
  Result := TParamYCoeff;
end;

constructor TYCoeffParamEditor.Create(AValue: Integer);
begin
  Inherited Create('Скругление по Y: ',  AValue);
end;

{ TXCoeffParamEditor }

function TXCoeffParamEditor.GetParamType: TParamClass;
begin
  Result := TParamXCoeff;
end;

constructor TXCoeffParamEditor.Create(AValue: Integer);
begin
  Inherited Create('Скругление по X: ', AValue);
end;

{ TIntegerParamEditor }

procedure TIntegerParamEditor.AttachParams(AParams: TParamArr;
  AGCotnrol: TGraphicControl);
begin
  Inherited;
  (FComponent as TSpinEdit).OnChange := @OnIntegerChange;
end;

procedure TIntegerParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue((FComponent as TSpinEdit).Value);
end;

function TIntegerParamEditor.GetParamType: TParamClass;
begin
  Result := TParamInteger;
end;

procedure TIntegerParamEditor.OnIntegerChange(Sender: TObject);
var i: Integer;
begin
  for i := Low(FParams) to High(FParams) do
    FParams[i].SetValue((Sender as TSpinEdit).Value);
  FGControl.Invalidate;
end;

constructor TIntegerParamEditor.Create(ACaption: String; AValue: Integer = 1);
begin
  Inherited Create;
  FLabel.Caption := ACaption;
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 1000;
    MinValue := 1;
    Value := AValue;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 130;
    //OnChange := @OnFactorChange;
  end;
end;

{ TCornersParamEdtitor }

procedure TCornersParamEdtitor.AttachParams(AParams: TParamArr;
  AGCotnrol: TGraphicControl);
begin
  Inherited;
  (FComponent as TSpinEdit).OnChange := @OnCornersNumberChange;
end;

procedure TCornersParamEdtitor.SetParam(AParam: TParam);
begin
  AParam.SetValue((FComponent as TSpinEdit).Value);
end;

function TCornersParamEdtitor.GetParamType: TParamClass;
begin
  Result := TParamCorners;
end;

procedure TCornersParamEdtitor.OnCornersNumberChange(Sender: TObject);
var i: Integer;
begin
  for i := Low(FParams) to High(FParams) do
    FParams[i].SetValue((Sender as TSpinEdit).Value);
  FGControl.Invalidate;
end;

constructor TCornersParamEdtitor.Create;
begin
  Inherited Create;
  FLabel.Caption := 'Количество углов';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 15;
    MinValue := 3;
    Value := 3;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    //OnChange := @OnCornersNumberChange;
  end;
end;

{ TBrushStyleParamEditor }

procedure TBrushStyleParamEditor.OnDrawBrushStyleItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    ARect.Top += 3;
    ARect.Left += 3;
    ARect.Right -= 3;
    ARect.Bottom -= 3;
    Rectangle(ARect);
    //workaround of not working bsClear
    if TFPBrushStyle(Index) = bsClear then begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
    end
    else begin
      Brush.Style := TFPBrushStyle(Index);
      Brush.Color := clBlack;
    end;
    if odFocused in State then
      Brush.Color := clBlue;
    Pen.Color := clBlack;
    Rectangle(ARect);
  end;
end;

procedure TBrushStyleParamEditor.AttachParams(AParams: TParamArr;
  AGCotnrol: TGraphicControl);
begin
  Inherited;
  (FComponent as TComboBox).OnChange := @OnBrushStyleChange;
end;

procedure TBrushStyleParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue(TFPBrushStyle((FComponent as TComboBox).ItemIndex));
end;

function TBrushStyleParamEditor.GetParamType: TParamClass;
begin
  Result := TParamBrushStyle;
end;

procedure TBrushStyleParamEditor.OnBrushStyleChange(Sender: TObject);
var i: Integer;
begin
  for i := Low(FParams) to High(FParams) do
    FParams[i].SetValue(TFPBrushStyle((Sender as TComboBox).ItemIndex));
  FGControl.Invalidate;
end;

constructor TBrushStyleParamEditor.Create;
var i: Integer;
begin
  Inherited Create;
  FLabel.Caption := 'Стиль заливки';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    for i := 0 to 7 do Items.Add('');
    Font.Size := 10;
    Width := 130;
    ItemIndex := 0;
    ReadOnly := True;
    AutoSelect := False;
    ItemHeight := 20;
    Style := csOwnerDrawFixed;
    OnDrawItem := @OnDrawBrushStyleItem;
    //OnChange := @OnBrushStyleChange;
  end;
end;

{ TLineStyleParamEditor }

procedure TLineStyleParamEditor.OnDrawLineStyleItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Style := psClear;
    Pen.Color := clWhite;
    ARect.Top += 3;
    ARect.Left += 3;
    ARect.Right -= 3;
    ARect.Bottom -= 3;
    if odFocused in State then
      Brush.Color := clBlue;
    Rectangle(ARect);
    Pen.Style := TFPPenStyle(Index);
    Pen.Width := 3;
    Pen.Color := clBlack;
    Line(ARect);
  end;
end;

procedure TLineStyleParamEditor.AttachParams(AParams: TParamArr;
  AGCotnrol: TGraphicControl);
begin
  Inherited;
  (FComponent as TComboBox).OnChange := @OnLineStyleChange;
end;

procedure TLineStyleParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue(TFPPenStyle((FComponent as TComboBox).ItemIndex));
end;

function TLineStyleParamEditor.GetParamType: TParamClass;
begin
  Result := TParamLineStyle;
end;

procedure TLineStyleParamEditor.OnLineStyleChange(Sender: TObject);
var i: Integer;
begin
  for i := Low(FParams) to High(FParams) do
    FParams[i].SetValue(TFPPenStyle((Sender as TComboBox).ItemIndex));
  FGControl.Invalidate;
end;

constructor TLineStyleParamEditor.Create;
var i: Integer;
begin
  Inherited Create;
  FLabel.Caption := 'Стиль линии';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    for i := 0 to 4 do Items.Add('');
    Style := csOwnerDrawFixed;
    ReadOnly := True;
    Width := 130;
    ItemIndex := 0;
    ItemHeight := 20;
    OnDrawItem := @OnDrawLineStyleItem;
    //OnChange := @OnLineStyleChange;
  end;
end;

{ TParamEditor }

procedure TParamEditor.AttachParams(AParams: TParamArr;
  AGCotnrol: TGraphicControl);
begin
  FParams := AParams;
  FGControl := AGCotnrol;
end;

constructor TParamEditor.Create;
begin
  Inherited;
  FLabel := TLabel.Create(nil);
  FLabel.Font.Size := 11;
end;

destructor TParamEditor.Destroy;
begin
  FLabel.Free;
  FComponent.Free;
  inherited Destroy;
end;

{ TLineWidthParamEditor }

procedure TLineWidthParamEditor.AttachParams(AParams: TParamArr;
  AGCotnrol: TGraphicControl);
begin
  Inherited;
  (FComponent as TSpinEdit).OnChange := @OnLineWidthChange;
end;

procedure TLineWidthParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue((FComponent as TSpinEdit).Value);
end;

function TLineWidthParamEditor.GetParamType: TParamClass;
begin
  Result := TParamLineWidth;
end;

procedure TLineWidthParamEditor.OnLineWidthChange(Sender: TObject);
var i: Integer;
begin
  for i := Low(FParams) to High(FParams) do
    FParams[i].SetValue((Sender as TSpinEdit).Value);
  FGControl.Invalidate;
end;

constructor TLineWidthParamEditor.Create;
begin
  Inherited Create;
  FLabel.Caption := 'Толщина линии';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 500;
    MinValue := 1;
    Value := 3;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    //OnChange := @OnLineWidthChange;
  end;
end;

end.

