package ui

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"html"
	"log"
	"net/url"
	"runtime"
	"sort"
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
	// f.calcHash()
	runtime.GC()
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

func (f *TFormMain) OnButtonURLBase64EncodeClick(sender vcl.IObject) {
	input := f.MemoCoderInput.Text()
	output := base64.URLEncoding.EncodeToString([]byte(input))
	f.MemoCoderOutput.SetText(output)
}

func (f *TFormMain) OnButtonURLBase64DecodeClick(sender vcl.IObject) {
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
	f.autoUpdateWeather()
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
			vcl.ThreadSync(func() {
				now := time.Now()
				s := now.Format("01月02日 #WeekDay# 15:04:05")
				s = strings.ReplaceAll(s, "#WeekDay#", WeekDayMap[now.Weekday().String()])
				f.StatusBarName.Panels().Items(3).SetText(s)
			})
			<-timer.C
		}
	}()
}

func (f *TFormMain) autoUpdateWeather() {
	fn := func() {
		w, err := gotil.GetWeather()
		if err != nil {
			log.Println(err)
			return
		}
		vcl.ThreadSync(func() {
			f.StatusBarName.Panels().Items(0).SetText(w.Location.IP)
			s := fmt.Sprintf("%s %s-%s %s %s更新", w.Location.Country, w.Temp1, w.Temp2, w.Weather, w.Time)
			f.StatusBarName.Panels().Items(1).SetText(s)
		})
	}
	go func() {
		timer := time.NewTicker(time.Minute * 15)
		for {
			fn()
			<-timer.C
		}
	}()
}

// ---------------------------------------------------------------------------------------------------------------------
// JSON
// ---------------------------------------------------------------------------------------------------------------------

func (f *TFormMain) OnButtonJSONFormatClick(sender vcl.IObject) {
	input := f.MemoJSONText.Text()
	bf := new(bytes.Buffer)
	var output string
	if err := json.Indent(bf, []byte(input), "", "    "); err != nil {
		output = err.Error()
	} else {
		output = bf.String()
	}
	f.MemoJSONText.SetText(output)
}

func (f *TFormMain) OnButtonJSONCompactClick(sender vcl.IObject) {
	input := f.MemoJSONText.Text()
	bf := new(bytes.Buffer)
	var output string
	if err := json.Compact(bf, []byte(input)); err != nil {
		output = err.Error()
	} else {
		output = bf.String()
	}
	f.MemoJSONText.SetText(output)
}

func (f *TFormMain) OnButtonJSONClearClick(sender vcl.IObject) {
	f.MemoJSONText.Clear()
	f.TreeViewJSONView.Items().Clear()
}

func (f *TFormMain) OnMemoJSONTextChange(sender vcl.IObject) {
	f.TreeViewJSONView.Items().Clear()
	f.jsonTree(f.MemoJSONText.Text())
}

func (f *TFormMain) jsonTree(s string) {
	s = strings.TrimSpace(s)
	if s == "" {
		return
	}
	var data interface{}
	if err := json.Unmarshal([]byte(s), &data); err != nil {
		return
	}
	f.TreeViewJSONView.Items().BeginUpdate()
	defer f.TreeViewJSONView.Items().EndUpdate()
	root := f.TreeViewJSONView.Items().AddChild(nil, "JSON")
	f.buildTree(root, data, "")
	f.TreeViewJSONView.FullExpand()
}

func (f *TFormMain) buildTree(node *vcl.TTreeNode, data interface{}, keyName string) {
	switch data.(type) {
	case map[string]interface{}:
		var child *vcl.TTreeNode
		if keyName != "" {
			child = f.TreeViewJSONView.Items().AddChild(node, keyName)
			child = f.TreeViewJSONView.Items().AddChild(child, "Object")
		} else {
			child = f.TreeViewJSONView.Items().AddChild(node, "Object")
		}
		keys := make([]string, 0)
		for key := range data.(map[string]interface{}) {
			keys = append(keys, key)
		}
		sort.Strings(keys)
		for _, key := range keys {
			if val, ok := data.(map[string]interface{})[key]; ok {
				f.buildTree(child, val, key)
			}
		}
	case []interface{}:
		var child *vcl.TTreeNode
		if keyName != "" {
			child = f.TreeViewJSONView.Items().AddChild(node, keyName)
			child = f.TreeViewJSONView.Items().AddChild(child, "Array")
		} else {
			child = f.TreeViewJSONView.Items().AddChild(node, "Array")
		}
		for _, val := range data.([]interface{}) {
			f.buildTree(child, val, "")
		}
	default:
		if node != nil && node.IsValid() {
			if keyName == "" {
				f.TreeViewJSONView.Items().AddChild(node, fmt.Sprintf("%v", data))
			} else {
				f.TreeViewJSONView.Items().AddChild(node, fmt.Sprintf(`%s: %v`, keyName, data))
			}
		}
	}
}
