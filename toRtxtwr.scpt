global myapp, editor, CurrLine
set myapp to "R"
--set myapp to "R"
--set myapp to ""

set editor to "TextWrangler"

set CurrLine to my getCurrentLine()

if myapp is equal to "R" then
	tell application "R"
		activate
		cmd CurrLine
	end tell
else if myapp is equal to "" then
else
	display dialog "Don't know how to send lines to " & myapp
end if

to getCurrentLine() -- gets either the selected text or the line the cursor is on
	tell window 1 of application editor
		set sel to selection
		set SelLength to length of sel
		
		if SelLength > 0 then
			return sel as string
		else
			try
				set lineNum to startline of sel
			on error
				-- startline is sometimes missing from sel. Access it by the class name instead
				set lineNum to «class SLin» of sel
			end try
			set lineTxt to line lineNum as string
			return lineTxt
		end if
	end tell
end getCurrentLine