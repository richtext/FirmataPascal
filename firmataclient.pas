unit FirmataClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazSerial, FirmataConstants, ExtCtrls;

type
  TPin = record
    mode            : byte;
    analog_channel  : byte;
    supported_modes : UInt64;
    value           : integer;
  end;

type
  Firmata = class
  private
   //vars
   LazSerial         : TLazSerial;
   Timer             : TTimer;

   buf               : array[0..64] of byte;
   pin_info          : array[0..128] of TPin;
   rx_count          : integer;
   tx_count          : integer;

   parse_count       : integer;
   parse_command_len : integer;
   parse_buf         : array[0..4096] of byte;

   PFirmwareName      : string;

   //methods
   procedure firmataParser(buffer : array of byte; len : integer);
   procedure DecodeMessage();
   procedure askBoardCapabilities();
   procedure askFirmware();

   procedure initPinInfo();
   procedure initLazSerial(port : string);
   procedure initTimer();
   procedure TimedRead(Sender: TObject);

  public
   //vars

   //methods
   constructor Create(port: string);
   destructor Destroy; override;

   procedure setPinMode( pin: byte; mode: byte);
   procedure digitalWrite( pin: byte; value: byte);
   procedure analogWrite(pin: byte; value: byte);
   procedure digitalReport(port: byte; enab: boolean);
   procedure analogReport(pin: byte; enab: boolean);
   function  digitalRead(pin:integer) : integer;
   function  analogRead(pin:integer)  : integer;

  published
   property FirmwareName : string read PFirmwareName write PFirmwareName;

  end;


implementation

constructor Firmata.Create(port : string);
begin
  initPinInfo();
  initLazSerial(port);
  initTimer();

  LazSerial.Active:= True;
  LazSerial.Open;
  Timer.Enabled:= true;

  askFirmware();

  {while LazSerial.DataAvailable = false do
  begin
    Application.ProcessMessages;
  end;}
  //safeWaiting
  sleep(2000);
end;

procedure Firmata.TimedRead(Sender: TObject);
var
   idx         : integer;
   Fmsg        : AnsiString;
   byteMessage : array[0..4096] of byte;
begin
    Fmsg := '';
    Fmsg := LazSerial.ReadData;

    if(Fmsg.IsNullOrEmpty(Fmsg)) then
    begin
         exit;
    end;
    rx_count+= (Fmsg.Length) ;
    //converts array of char to array of byte
    for idx:= 1 to (Fmsg.Length) do
    begin
      byteMessage[idx-1] := byte(Fmsg[idx]);
    end;
    //Call the parser
    firmataParser(byteMessage, Fmsg.Length)
end;

procedure Firmata.initPinInfo();
var
   count : integer;
begin
  for count:=0 to 127 do
  begin
    with pin_info[count] do
    begin
      mode:= 255;
      analog_channel := 127;
      supported_modes := 0;
      value:=0
    end;
  end;
end;

procedure Firmata.initLazSerial(port: string);
begin
  LazSerial:= TLazSerial.Create(nil);

  LazSerial.BaudRate:= br_57600;
  LazSerial.DataBits:=db8bits;
  LazSerial.Parity:=pNone;
  LazSerial.StopBits:=sbOne;
  LazSerial.FlowControl:=fcNone;
  LazSerial.Device:= port;
end;

procedure Firmata.initTimer();
begin
  Timer:= TTimer.Create(nil);

  Timer.Interval:=40;
  Timer.Name:='FirmataTimer';
  Timer.OnTimer:= @TimedRead;
end;

procedure Firmata.setPinMode( pin: byte; mode: byte);
begin
  buf[0] := SET_PIN_MODE;
  buf[1] := pin;
  buf[2] := mode;
  LazSerial.WriteBuffer(buf,3);
  pin_info[pin].mode:= mode;
  pin_info[pin].value:= 0;
  tx_count+=3;
  end;

procedure Firmata.digitalWrite( pin: byte; value: byte);
begin
  buf[0] := SET_DIGITAL_PIN_VALUE;
  buf[1] := pin;
  buf[2] := value;
  pin_info[pin].value:= value;
  LazSerial.WriteBuffer(buf,3);
  tx_count+=3;
  end;

procedure Firmata.analogWrite(pin: byte; value: byte);
begin
  buf[0] := ANALOG_MESSAGE OR pin;
  buf[1] := value AND $7F;
  buf[2] := value >> 7 AND $7F;
  pin_info[pin].value:= value;
  LazSerial.WriteBuffer(buf,3);
  tx_count+=3;
  end;

procedure Firmata.digitalReport(port: byte; enab: boolean);
begin
  buf[0] := REPORT_DIGITAL OR port;
  buf[1] := byte(enab);
  LazSerial.WriteBuffer(buf,2);
  tx_count+=2;
end;

procedure Firmata.analogReport(pin: byte; enab: boolean);
begin
  buf[0] := REPORT_ANALOG OR pin;
  buf[1] := byte(enab);
  LazSerial.WriteBuffer(buf,2);
  tx_count+=2;
end;

procedure Firmata.firmataParser(buffer : array of byte; len : integer);
var
   idx: integer;
   msn: byte;
begin
    for idx := 0 to len do
    begin
      msn := buffer[idx] and $F0;
      if ((msn = ANALOG_MESSAGE) or (msn = DIGITAL_MESSAGE) or (msn = REPORT_VERSION)) then
      begin
        parse_command_len := 3;
	parse_count := 0;
      end
      else if ((msn = REPORT_ANALOG) or (msn = REPORT_DIGITAL) ) then
      begin
        parse_command_len := 2;
	parse_count := 0;
      end
      else if (buffer[idx] = START_SYSEX) then
      begin
        parse_count := 0;
	parse_command_len := sizeof(parse_buf);
      end
      else if(buffer[idx] = END_SYSEX) then
      begin
        parse_command_len := parse_count + 1;
      end
      else if (buffer[idx] and $80 > 0) then
      begin
        parse_command_len := 1;
	parse_count := 0;
      end;

      if(parse_count < SizeOf(parse_buf) ) then
      begin
        parse_buf[parse_count] := buffer[idx];
        parse_count:= parse_count + 1;
      end;

      if(parse_count = parse_command_len) then
      begin
        DecodeMessage();
        parse_count:=0;
        parse_command_len:=0;
      end;
    end;

end;

procedure Firmata.DecodeMessage();
var
   cmd        : byte;
   analog_ch  : integer;
   analog_val : integer;
   port_num   : integer;
   port_val   : integer;
   pin        : integer; //contador
   val        : integer;
   mask       : byte;
   firmName   : array of char;
   len        : integer;
   idx        : integer; //contador
   n          : integer;
begin

  cmd:= (parse_buf[0] and $F0);

  if( (cmd = ANALOG_MESSAGE) and (parse_count = 3) ) then
  begin
    analog_ch := parse_buf[0] and $0F;
    analog_val:= parse_buf[1] or (parse_buf[2] << 7);

    for pin:=0 to 127 do
    begin
      if(pin_info[pin].analog_channel = analog_ch) then
      begin
        pin_info[pin].value := analog_val;
        exit;
      end;

    end;
    exit;
  end;

  if( (cmd = DIGITAL_MESSAGE) and (parse_count = 3) ) then
  begin
    port_num:= (parse_buf[0] and $0F);
    port_val:= parse_buf[1] or (parse_buf[2] << 7);
    mask    := $01;

    for pin := (port_num * 8) to ((port_num * 8) + 7) do
    begin
      if( (pin_info[pin].mode = PIN_MODE_INPUT) or (pin_info[pin].mode = PIN_MODE_PULLUP) ) then
      val := (port_val and mask);
      if val > 0 then val:= 1;

      if (pin_info[pin].value <> val) then
      begin
        pin_info[pin].value := val;
      end;

      mask := mask << 1;
    end;

    exit;

  end;
  //sysex message
  if ( (Parse_buf[0] = START_SYSEX) and (parse_buf[parse_count - 1] = END_SYSEX) ) then
  begin
    if(parse_buf[1] = REPORT_FIRMWARE) then
    begin
      len:= 0;
      idx:= 4;
      SetLength(firmName, 160);
      while idx < (parse_count - 2) do
      begin
        firmName[len] := char( ( (parse_buf[idx] and $7F) or ( (parse_buf[idx + 1] and $7F) << 7 ) ) );
        len+=1;
        idx+=2;
      end;

      firmName[len]:= '-';
      len+=1;
      firmName[len]:= char(parse_buf[2] + $30); // $30 is ASCII CODE FOR ZERO NUMBER
      len+=1;
      firmName[len]:= '.';
      len+=1;
      firmName[len]:= char(parse_buf[3]+ $30);
      len+=1;
      firmName[len]:= char(0);

      SetString(PFirmwareName,PChar(@firmName[0]), Length(firmName));

      // query the board's capabilities only after hearing the
      // REPORT_FIRMWARE message.  For boards that reset when
      // the port open (eg, Arduino with reset=DTR), they are
      // not ready to communicate for some time, so the only
      // way to reliably query their capabilities is to wait
      // until the REPORT_FIRMWARE message is heard.
      askBoardCapabilities();

    end
    else if ( parse_buf[1] = CAPABILITY_RESPONSE) then
    begin
      for pin:= 0 to 127 do
      begin
        pin_info[pin].supported_modes:= 0;
      end;

      idx:= 2;
      n := 0;
      pin := 0;

      while idx < parse_count do
      begin
        if(parse_buf[idx] = 127) then
        begin
          pin+=1;
          n := 0;
        end;
        if (n = 0) then
        begin
          pin_info[pin].supported_modes:= pin_info[pin].supported_modes or (1 << parse_buf[idx]);
        end;
        n:= n xor 1;
        idx+=1;
      end;
      // send a state query for for every pin with any modes
      for pin:=0 to 127 do
      begin
        len:=0;
        if(pin_info[pin].supported_modes > 0) then
        begin
          buf[0] := START_SYSEX;
	  buf[1] := PIN_STATE_QUERY;
	  buf[2] := pin;
	  buf[3] := END_SYSEX;
          len := 4;
          LazSerial.WriteBuffer(buf, len);
          tx_count+= len;
        end;
      end;
    end
    else if (parse_buf[1] = ANALOG_MAPPING_RESPONSE) then
    begin
      pin:= 0;
      for idx := 2 to (parse_count - 1) do
      begin
        pin_info[pin].analog_channel:= parse_buf[idx];
        pin+=1;
      end;
      exit;
    end
    else if ( (parse_buf[1] = PIN_STATE_RESPONSE) and (parse_count >=6) ) then
    begin
      pin := parse_buf[2];
      pin_info[pin].mode := parse_buf[3];
      pin_info[pin].value := parse_buf[4];
      if(parse_count > 6) then
      begin
        pin_info[pin].value:= (pin_info[pin].value) or (parse_buf[5] << 7);
      end;
      if(parse_count > 7) then
      begin
        pin_info[pin].value:= (pin_info[pin].value) or (parse_buf[6] << 14);
      end;
      exit;
    end;
  end
end;

function  Firmata.digitalRead(pin: integer) : integer;
begin
   digitalRead := pin_info[pin].value;
end;

function  Firmata.analogRead(pin:integer)  : integer;
begin
   analogRead := pin_info[pin].value;
end;

procedure Firmata.askBoardCapabilities();
var
   len : integer;
   idx : integer;
begin
  buf[0] := START_SYSEX;
  buf[1] := ANALOG_MAPPING_QUERY;
  buf[2] := END_SYSEX;
  buf[3] := START_SYSEX;
  buf[4] := CAPABILITY_QUERY;
  buf[5] := END_SYSEX;
  len:= 6;
  for idx:= 0 to 15 do
  begin
    len+=1;
    buf[len] := $C0 or idx;  // report analog
    len+=1;
    buf[len] := 1;
    len+=1;
    buf[len] := $D0 or idx;  // report digital
    len+=1;
    buf[len] := 1;
  end;

  LazSerial.WriteBuffer(buf,len);
  tx_count += len;
end;

procedure Firmata.askFirmware();
begin
  buf[0] := START_SYSEX;
  buf[1] := REPORT_FIRMWARE;
  buf[2] := END_SYSEX;
  LazSerial.WriteBuffer(buf,3);
  tx_count+= 3;
end;

destructor Firmata.Destroy();
begin

end;

end.

