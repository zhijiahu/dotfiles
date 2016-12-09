;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ed Cottrell's AutoHotKey script for toggling the "Find Results" pane/window in Notepad++
; Released under the MIT License (http://opensource.org/licenses/MIT)
; Version: 1.1
; Release Date: January 15, 2014
; Released on Superuser.com: http://superuser.com/questions/700357/create-a-hotkey-keyboard-shortcut-to-close-the-notepad-find-results-window
; Also released at www.edcottrell.com/2014/01/11/toggle-find-results-window-notepad-hotkey/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Turn F7 into a toggle for the Notepad++ search results window; currently it shows it, but doesn't hide it.
; The $ prevents this from firing itself
*$F7::
Open := 0
SetTitleMatchMode 2  ; AHK doesn't seem to recognize the window title otherwise
; See if Notepad++ is the active window or if the undocked results window (ahk_class #32770) is the active window
If WinActive("Notepad++")
{
    ; If the results pane is open, close it
    ; Button1 is the class name for the title bar and close button of the results pane when docked
    ControlGet, OutputVar, Visible,, Button1, Notepad++
    if ErrorLevel = 0
    {
        If OutputVar > 0
        {
            ; Found it docked
            Open := 1
            ; Get the size and coordinates of the title bar and button
            ControlGetPos, X, Y, Width, Height, Button1
            ; Set the coordinates of the close button
            X := Width - 9
            Y := 5
            ; Send a click
            ControlClick, Button1,,,,, NA x%X% y%Y%
        }
    }
}
; If it is undocked, use ahk_class #32770
else If WinExist("Find result ahk_class #32770")
{
    ; Found it undocked
    Open := 1
    ; Close it
    WinClose
}
; It's not open, so open it
if Open = 0
{
    SendInput {F7}
}
return