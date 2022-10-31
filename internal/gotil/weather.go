package gotil

import (
	"encoding/json"
	"errors"
	"io"
	"net/http"
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
	City    string `json:"city"`
	CityID  string `json:"cityid"`
	Temp1   string `json:"temp1"`
	Temp2   string `json:"temp2"`
	Weather string `json:"weather"`
	Img1    string `json:"img1"`
	Img2    string `json:"img2"`
	Time    string `json:"ptime"`
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
	url := "http"
	url += "://www.weather.com.cn/data/cityinfo/" + code + ".html"
	res, err := http.Get(url)
	if err != nil {
		return
	}
	body, err := io.ReadAll(res.Body)
	defer func() { _ = res.Body.Close() }()
	var r weatherComCN
	if err = json.Unmarshal(body, &r); err != nil {
		return
	}
	w.Location.IP = loc.IP
	w.Location.Country = loc.Country
	w.Location.IP = loc.IP
	w.City = r.WeatherInfo.City
	w.Temp1 = r.WeatherInfo.Temp1
	w.Temp2 = r.WeatherInfo.Temp2
	w.Weather = r.WeatherInfo.Weather
	w.Time = r.WeatherInfo.Time
	return
}
