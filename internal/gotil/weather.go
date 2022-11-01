package gotil

import (
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"
)

type Weather struct {
	Location
	City    string
	Temp1   string
	Temp2   string
	Weather string
	Time    string
}

type weatherComCN struct {
	WeatherInfo weatherInfo `json:"weatherinfo"`
}

type weatherInfo struct {
	City         string `json:"cityname"`
	CityID       string `json:"city"`
	FcTime       string `json:"fctime"`
	Temp1        string `json:"tempn"`
	Temp2        string `json:"temp"`
	Weather      string `json:"weather"`
	WeatherCode  string `json:"weathercode"`
	WeatherCodeN string `json:"weathercoden"`
	Wd           string `json:"wd"`
	Ws           string `json:"ws"`
}

func GetWeather() (w Weather, err error) {
	defer func() {
		if err != nil {
			err = errors.New("获取天气预报失败: " + err.Error())
			return
		}
	}()
	loc, err := GetLocation()
	if err != nil {
		return
	}
	code, err := getWeatherCityCode(loc.Country)
	if err != nil {
		return
	}
	t := strconv.Itoa(int(time.Now().UnixMilli()))
	req, err := http.NewRequest(http.MethodGet, "https://d1.weather.com.cn/dingzhi/"+code+".html?_="+t, nil)
	if err != nil {
		return
	}
	req.Header.Set("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:97.0) Gecko/20100101 Firefox/97.0")
	req.Header.Set("Referer", "https://www.weather.com.cn/")
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return
	}
	data, err := io.ReadAll(res.Body)
	body := string(data)
	a, b := strings.Index(body, "="), strings.Index(body, ";")
	if a != -1 && b != -1 && a < b && a+1 < len(body) && b < len(body) {
		body = body[a+1 : b]
	}
	defer func() { _ = res.Body.Close() }()
	var r weatherComCN
	if err = json.Unmarshal([]byte(body), &r); err != nil {
		return
	}
	w.Location.IP = loc.IP
	w.Location.Country = loc.Country
	w.Location.IP = loc.IP
	w.City = r.WeatherInfo.City
	w.Temp1 = r.WeatherInfo.Temp1
	w.Temp2 = r.WeatherInfo.Temp2
	w.Weather = r.WeatherInfo.Weather
	// if r.WeatherInfo.Ws != "" {
	// 	w.Weather += " " + r.WeatherInfo.Ws
	// }
	// if r.WeatherInfo.Wd != "" {
	// 	w.Weather += " " + r.WeatherInfo.Wd
	// }
	if len(r.WeatherInfo.FcTime) == 12 {
		w.Time = r.WeatherInfo.FcTime[8:10] + ":00"
	} else {
		w.Time = "未知"
	}
	return
}
