unit main;
{Список вопросов:
 Почему FindClass возвращает TPersistent? Какой вообще смысл в этой функции?
 Можно ли избавиться от case в load?

 Почему падает при даблклике?


 procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterClasses([TButton, TForm]);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  CRef : TPersistentClass;
  AControl : TControl;
begin
  CRef := GetClass('TButton');
  if CRef<>nil then
  begin
     AControl := TControl(TControlClass(CRef).Create(Self));
     with AControl do
     begin
        Parent := Self;
        Width := 50;
        Height := 30;
     end;
  end;
end;

 }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Math, Dialogs, Menus,
  ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, Buttons, ActnList, Grids,
  UFigures, UTools, UTransform, Types, UAppState, USaveLoad;

type

  { TVectorEditor }
  TVectorEditor = class(TForm)
    ClearFigures: TAction;
    FileDivider2MenuItem: TMenuItem;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    SaveFileAsMenuItem: TMenuItem;
    SaveFileMenuItem: TMenuItem;
    SaveFileAsAction: TAction;
    OpenFileAction: TAction;
    SaveFileAction: TAction;
    ActionList: TActionList;
    ColorDialog: TColorDialog;
    ImageBoundsLabel: TLabel;
    ImageBoundsX: TLabel;
    ImageBoundsY: TLabel;
    ColorPanel: TPanel;
    OpenFileMenuItem: TMenuItem;
    ToolButtonPanel: TPanel;
    ParamPanel: TPanel;
    ScrollbarMinLabel: TLabel;
    ScrollbarMaxLabel: TLabel;
    ScrollbarPosLabel: TLabel;
    ScrollbarsLabel: TLabel;
    MouseWrldLabel: TLabel;
    MouseXWrldLabel: TLabel;
    MouseYWrldLabel: TLabel;
    MouseYDspLabel: TLabel;
    MouseXDspLabel: TLabel;
    MouseDspLabel: TLabel;
    OffsetLabel: TLabel;
    OffsetYLabel: TLabel;
    OffsetXLabel: TLabel;
    PercentLabel: TLabel;
    DebugPanel: TPanel;
    ScaleLabel: TLabel;
    ShowEverythingMenuItem: TMenuItem;
    ScaleFloatSpinEdit: TFloatSpinEdit;
    PaletteGrid: TDrawGrid;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileDividerMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    BrushColorPanel: TPanel;
    PenColorPanel: TPanel;
    HorizontalScrollBar: TScrollBar;
    VerticalScrollBar: TScrollBar;
    ToolPanel: TPanel;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ClearFiguresExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure SaveFileActionExecute(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure PaletteGridDblClick(Sender: TObject);
    procedure PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveFileAsActionExecute(Sender: TObject);
    procedure ScaleFloatSpinEditChange(Sender: TObject);
    procedure ShowEverythingMenuItemClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure CreateToolsButtons(ABtnWidth, ABtnHeight, AColsCount: Integer);
    procedure FillPalette;
    procedure SetScrollBarsPostions;
    procedure SaveFigure(Figure: TFigure);
    procedure RedefineImageBounds;
    procedure UpdateScale;
    procedure VerticalScrollBarChange(Sender: TObject);
    procedure UpdateDimensions;
  private
    { private declarations }

  public
    { public declarations }

  end;

var
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

var
  PenColor: TColor = clBlack;
  BrushColor: TColor = clBlue;
  isDrawing: Boolean = False;
  ImageBounds: TDoubleRect;
  PaletteColors: array of array of TColor;
  CurrentTool: TTool;

{ TVectorEditor }

procedure TVectorEditor.UpdateScale;
begin
  ScaleFloatSpinEdit.Value := Scale * 100;
end;

procedure TVectorEditor.SaveFigure(Figure: TFigure);
begin
  if Figure <> nil then begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := Figure;
  end;
end;

procedure TVectorEditor.RedefineImageBounds;
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
      with Figures[i].GetBounds do begin
        if i = 0 then begin
          ImageBounds.Left := Left;
          ImageBounds.Top := Top;
          ImageBounds.Right := Right;
          ImageBounds.Bottom := Bottom;
        end
        else begin
          ImageBounds.Left := Min(ImageBounds.left, Left);
          ImageBounds.Top := Min(ImageBounds.Top, Top);
          ImageBounds.Right := Max(ImageBounds.Right, Right);
          ImageBounds.Bottom := Max(ImageBounds.Bottom, Bottom);
        end;
      end;
  end;
end;

procedure TVectorEditor.UpdateDimensions;
begin
  DispDimensions := Dimensions(
    PaintBox.ClientWidth - 1,
    PaintBox.ClientHeight - 1);
end;

procedure TVectorEditor.SetScrollBarsPostions;
var
  HorPosition, HorMin, HorMax, HorPageSize: Integer;
  VertPosition, VertMin, VertMax, VertPageSize: Integer;
begin
  HorPosition := round(GetCanvasOffset.X);
  HorPageSize := DispDimensions.Width;
  HorMin := WorldToDispDimension(ImageBounds.Left);
  if HorPosition < HorMin then
    HorMin := HorPosition;
  HorMax :=  WorldToDispDimension(ImageBounds.Right);
  if HorPosition > HorMax - HorPageSize then
    HorMax := HorPosition + HorPageSize;
  HorizontalScrollBar.SetParams(HorPosition, HorMin, HorMax, HorPageSize);

  VertPosition := round(GetCanvasOffset.Y);
  VertPageSize := DispDimensions.Height;
  VertMin := WorldToDispDimension(ImageBounds.Top);
  if VertPosition < VertMin then
    VertMin := VertPosition;
  VertMax :=  WorldToDispDimension(ImageBounds.Bottom);
  if VertPosition > VertMax - VertPageSize then
    VertMax := VertPosition + VertPageSize;
  VerticalScrollBar.SetParams(VertPosition, VertMin, VertMax, VertPageSize);
 end;

procedure TVectorEditor.HorizontalScrollBarChange(Sender: TObject);
begin
  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
      Exit;
    end;
  end;

  with Sender as TScrollBar do
    SetCanvasOffset(Position, GetCanvasOffset.Y);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.VerticalScrollBarChange(Sender: TObject);
begin
  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
      Exit;
    end;
  end;
  with Sender as TScrollBar do
    SetCanvasOffset(GetCanvasOffset.X, Position);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ToolClick(Sender: TObject);
begin
  CurrentTool := Tools[(Sender as TSpeedButton).Tag];
  CurrentTool.Init(ParamPanel)
end;

procedure TVectorEditor.CreateToolsButtons(
  ABtnWidth, ABtnHeight, AColsCount: Integer);
var
  i: Integer;
  ToolBtn: TSpeedButton;
  ToolIcon: TBitmap;
begin
  for i := 0 to High(Tools) do begin
    ToolBtn := TSpeedButton.Create(VectorEditor);
    ToolIcon := TBitmap.Create;
    with ToolIcon do begin
      TransparentColor := clWhite;
      Transparent := True;
      LoadFromFile(Tools[i].Icon);
    end;
    with ToolBtn do begin
      Glyph := ToolIcon;
      Flat := True;
      Width := ABtnWidth;
      Height := ABtnHeight;
      Top := (i div AColsCount) * ABtnHeight;
      Left := (i mod AColsCount) * ABtnWidth;
      Tag := i;
      GroupIndex := 1;
      OnClick := @ToolClick;
      if i = 0 then Down := True;
      Parent := ToolButtonPanel;
    end;
  end;
end;

procedure TVectorEditor.FillPalette;
var
  col, row, rate, index: Integer;
begin
  index := 0;
  rate := floor(255 / (PaletteGrid.RowCount * PaletteGrid.ColCount));
  for row := 0 to PaletteGrid.RowCount do begin
    SetLength(PaletteColors, Length(PaletteColors) + 1);
    for col := 0  to PaletteGrid.ColCount do begin
      SetLength(PaletteColors[row], Length(PaletteColors[row]) + 1);
      PaletteColors[row, col] := RGBToColor(index * rate, col * 28,
        (PaletteGrid.ColCount - row) * 42);
      index += 1;
    end;
  end;
end;

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.FormCreate(Sender: TObject);
var
  BtnWidth, BtnHeight, ColsCount: Integer;
begin
  InitSaveLoad(Self);
  CurrentTool := Tools[0];
  CurrentTool.Init(ParamPanel);
  //Передаём дефолтный параметры представлению
  PenColorPanel.Color := PenColor;
  BrushColorPanel.Color := BrushColor;
  ScaleFloatSpinEdit.Value := 100;
  //Параметры кнопок  интрументов
  BtnWidth := 48;
  BtnHeight := 48;
  ColsCount := 3;
  CreateToolsButtons(BtnWidth, BtnHeight, ColsCount);
  //Палитра
  FillPalette;
  UpdateDimensions;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := True;
  CurrentTool.MouseDown(Point(X, Y), PenColor, BrushColor, Button, Shift, PaintBox);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isDrawing then begin
    CurrentTool.MouseMove(Point(X, Y));
    PaintBox.Invalidate;
  end;
  {DEBUG}
  MouseXDspLabel.Caption := 'x: ' + FloatToStr(X);
  MouseYDspLabel.Caption := 'y: ' + FloatToStr(Y);
  MouseXWrldLabel.Caption := 'x: ' + FloatToStr(DispToWorldCoord(X,Y).X);
  MouseYWrldLabel.Caption := 'y: ' + FloatToStr(DispToWorldCoord(X,Y).Y);
end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := False;
  CurrentTool.MouseUp(Point(X, Y));
  SaveFigure(CurrentTool.GetFigure);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos);
  DecreaseScale;
  AddCanvasOffset((StartingMouseCrds - DispToWorldCoord(MousePos)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos);
  IncreaseScale;
  AddCanvasOffset((StartingMouseCrds - DispToWorldCoord(MousePos)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do begin
    if Figures[i] <> Nil then
      Figures[i].Draw(PaintBox.Canvas);
  end;
  if isDrawing and (CurrentTool.GetFigure <> nil) then begin
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
  end;
  RedefineImageBounds;
  UpdateScale;
  SetScrollBarsPostions;
  {DEBUG}
  OffsetXLabel.Caption := 'x: ' + FloatToStr(GetCanvasOffset.X);
  OffsetYLabel.Caption := 'y: ' + FloatToStr(GetCanvasOffset.Y);
  ImageBoundsX.Caption := 'left: ' + FloatToStr(ImageBounds.Left);
  ImageBoundsY.Caption := 'top: ' + FloatToStr(ImageBounds.Top);
  ScrollbarMinLabel.Caption := 'Min: ' + IntToStr(HorizontalScrollBar.Min);
  ScrollbarMaxLabel.Caption := 'Max: ' + IntToStr(HorizontalScrollBar.Max);
  ScrollbarPosLabel.Caption := 'Pos: ' + IntToStr(HorizontalScrollBar.Position);
end;

procedure TVectorEditor.PaintBoxResize(Sender: TObject);
begin
  UpdateDimensions;
end;

procedure TVectorEditor.PaletteGridDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
    with Sender as TDrawGrid do begin
      PaletteColors[Row, COl] := ColorDialog.Color;
    end;
    PaletteGrid.Invalidate;
  end;
end;

procedure TVectorEditor.PaletteGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  PaletteGrid.Canvas.Brush.Color := PaletteColors[aRow, aCol];
  PaletteGrid.Canvas.FillRect(aRect);
end;

procedure TVectorEditor.PaletteGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  PaletteGrid.MouseToCell(X, Y, ACol, ARow);
  if Button = mbLeft then  begin
    PenColor := PaletteColors[ARow, ACol];
    PenColorPanel.Color := PenColor;
  end;
  if Button = mbRight then begin
    BrushColor := PaletteColors[ARow, ACol];
    BrushColorPanel.Color := BrushColor;
  end;
end;

procedure TVectorEditor.ScaleFloatSpinEditChange(Sender: TObject);
var
  CenterDspCrds: TPoint;
  StartCenterWrldCrds: TDoublePoint;
begin
  CenterDspCrds := Point(
    round(DispDimensions.Width  / 2), round(DispDimensions.Height / 2));
  StartCenterWrldCrds := DispToWorldCoord(CenterDspCrds);
  Scale := (Sender as TFloatSpinEdit).Value / 100;
  AddCanvasOffset((StartCenterWrldCrds - DispToWorldCoord(CenterDspCrds)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ShowEverythingMenuItemClick(Sender: TObject);
const
  BorderMargin = 5;//px
var
  XScale, YScale: Double;
  ImgWorldWidth, ImgWorldHeight: Double;
begin
  ImgWorldWidth := ImageBounds.Right - ImageBounds.Left;
  ImgWorldHeight := ImageBounds.Bottom - ImageBounds.Top;
  XScale := DispDimensions.Width / (ImgWorldWidth + 2 * BorderMargin / Scale);
  YScale := DispDimensions.Height / (ImgWorldHeight + 2 * BorderMargin / Scale);
  Scale := Min(XScale, YScale);
  //Размещение по центру
  SetCanvasOffset(
    WorldToDispDimension(ImageBounds.Left) -
      (DispDimensions.Width - WorldToDispDimension(ImgWorldWidth)) / 2,
    WorldToDispDimension(ImageBounds.Top) -
      (DispDimensions.Height - WorldToDispDimension(ImgWorldHeight)) / 2);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.ClearFiguresExecute(Sender: TObject);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do
    Figures[i].Free;
  Figures := nil;
  //RedefineImageBounds(DoubleRect(0, 0, 0, 0)); работает несовсем правильно
  SetCanvasOffset(0, 0);
  Scale := 1.0;
  PaintBox.Invalidate;
end;

procedure TVectorEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  UserAnswer: Integer;
begin
  if GetAppState = apsModified then begin
    UserAnswer := MessageDlg('Сохранить?','Сохранить файл перед выходом?', mtConfirmation,[mbYes,mbNo,mbCancel],0);
    if UserAnswer = mrYes then SaveFileAction.Execute
    else if UserAnswer = mrNo then CanClose := True
    else if  UserAnswer = mrCancel then CanClose := False;
  end;
end;

procedure TVectorEditor.OpenFileActionExecute(Sender: TObject);
var
  ImgWorldWidth: Double;
  ImgWorldHeight: Double;
begin
  //сделать предупреждение о потере данныx
  if OpenFileDialog.Execute then begin
    if FileLoad(OpenFileDialog.FileName, Figures) then begin
      RedefineImageBounds;
      ImgWorldWidth := ImageBounds.Right - ImageBounds.Left;
      ImgWorldHeight := ImageBounds.Bottom - ImageBounds.Top;
      SetCanvasOffset(
        WorldToDispDimension(ImageBounds.Left) -
          (DispDimensions.Width - WorldToDispDimension(ImgWorldWidth)) / 2,
        WorldToDispDimension(ImageBounds.Top) -
          (DispDimensions.Height - WorldToDispDimension(ImgWorldHeight)) / 2);
        Invalidate;
    end
    else
      ShowMessage('Неподдерижваемый формат');
  end;
end;

procedure TVectorEditor.SaveFileActionExecute(Sender: TObject);
begin
  if GetFileState = fisSaved then
    SaveFile(GetFilePath, Figures)
  else
    SaveFileAsAction.Execute;
end;

procedure TVectorEditor.SaveFileAsActionExecute(Sender: TObject);
begin
  SaveFileDialog.FileName := GetFilePath;
  SaveFileDialog.InitialDir := GetCurrentDir;
  if SaveFileDialog.Execute then begin
    SaveFile(SaveFileDialog.FileName, Figures);
  end;

end;

end.
