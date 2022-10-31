package gotil

import (
	"errors"
	"io"
	"net/http"
	"strings"
)

type Location struct {
	IP      string
	Country string
	Area    string
}

func GetLocation() (loc Location, err error) {
	res, err := http.Get("https://myip.ipip.net/")
	if err != nil {
		err = errors.New("获取IP地址失败: " + err.Error())
		return
	}
	body, err := io.ReadAll(res.Body)
	defer func() { _ = res.Body.Close() }()
	data := string(body)
	data = strings.TrimPrefix(data, "当前 IP：")
	i := strings.Index(data, " 来自")
	if i == -1 {
		err = errors.New("获取IP地址失败")
		return
	}
	IP := strings.TrimSpace(data[:i])
	r := NewQQWry().Find(IP)
	loc.IP = r.IP
	loc.Country = r.Country
	loc.Area = r.Area
	return
}
