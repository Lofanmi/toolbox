package ui

import (
	"encoding/base64"
	"fmt"
	"html"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/Lofanmi/toolbox/internal/gotil"
	"github.com/ying32/govcl/vcl"
)

// TFormMainFields
// ::private::
type TFormMainFields struct {
}

func (f *TFormMain) OnMemoHashInputChange(sender vcl.IObject) {
	f.calcHash()
}

func (f *TFormMain) OnCheckBoxHashLowerUpperCaseChange(sender vcl.IObject) {
	f.calcHash()
}

func (f *TFormMain) calcHash() {
	input := f.MemoHashInput.Text()
	upper := f.CheckBoxHashLowerUpperCase.Checked()
	f.EditHashOutputMD5.SetText(gotil.MD5(input, upper))
	f.EditHashOutputCRC32.SetText(gotil.CRC32(input, upper))
	f.EditHashOutputSHA1.SetText(gotil.SHA1(input, upper))
	f.EditHashOutputSHA256.SetText(gotil.SHA256(input, upper))
	f.EditHashOutputSHA512.SetText(gotil.SHA512(input, upper))
	f.EditHashOutputFNV32.SetText(gotil.FNV32(input, upper))
	f.EditHashOutputFNV64.SetText(gotil.FNV64(input, upper))
	f.EditHashOutputFNV128.SetText(gotil.FNV128(input, upper))
}

func (f *TFormMain) OnButtonBase64EncodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output := base64.StdEncoding.EncodeToString([]byte(input))
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonBase64DecodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	data, err := base64.StdEncoding.DecodeString(input)
	var output string
	if err != nil {
		output = "[操作失败]" + err.Error()
	} else {
		output = string(data)
	}
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonBase64EncodeURLClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output := base64.URLEncoding.EncodeToString([]byte(input))
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonBase64DecodeURLClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	data, err := base64.URLEncoding.DecodeString(input)
	var output string
	if err != nil {
		output = "[操作失败]" + err.Error()
	} else {
		output = string(data)
	}
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonURLEncodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output := url.QueryEscape(input)
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonURLDecodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output, err := url.QueryUnescape(input)
	if err != nil {
		output = "[操作失败]" + err.Error()
	}
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonUnicodeEncodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	sb := strings.Builder{}
	for _, r := range input {
		s := fmt.Sprintf("\\u%X", r)
		sb.WriteString(strings.ToLower(s))
	}
	output := sb.String()
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonUnicodeDecodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output, err := strconv.Unquote(`"` + input + `"`)
	if err != nil {
		output = "[操作失败]" + err.Error()
	}
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonHTMLEncodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output := html.EscapeString(input)
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonHTMLDecodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output := html.UnescapeString(input)
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnFormCreate(sender vcl.IObject) {
	f.autoUpdateCurrentTime()
}

func (f *TFormMain) autoUpdateCurrentTime() {
	var WeekDayMap = map[string]string{
		"Monday":    "周一",
		"Tuesday":   "周二",
		"Wednesday": "周三",
		"Thursday":  "周四",
		"Friday":    "周五",
		"Saturday":  "周六",
		"Sunday":    "周日",
	}
	go func() {
		timer := time.NewTicker(time.Second)
		for {
			<-timer.C
			vcl.ThreadSync(func() {
				now := time.Now()
				s := now.Format("01月02日 #WeekDay# 15:04:05")
				s = strings.ReplaceAll(s, "#WeekDay#", WeekDayMap[now.Weekday().String()])
				f.StatusBarName.Panels().Items(2).SetText(s)
			})
		}
	}()
}

func (f *TFormMain) OnPanelCoderButtonGroupClick(sender vcl.IObject) {

}

