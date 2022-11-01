package main

import (
	"github.com/Lofanmi/toolbox/internal/gotil"
	_ "github.com/ying32/govcl/pkgs/macapp"
	_ "github.com/ying32/govcl/pkgs/winappres"

	"github.com/Lofanmi/toolbox/ui"
	"github.com/ying32/govcl/vcl"
)

func main() {
	if err := gotil.InitIPData("/Users/a37/code/github/Lofanmi/toolbox/qqwry.dat"); err != nil {
		panic(err)
	}
	vcl.Application.SetScaled(true)
	vcl.Application.SetTitle("扬扬工具箱")
	vcl.Application.Initialize()
	vcl.Application.SetMainFormOnTaskBar(true)
	vcl.Application.CreateForm(&ui.FormMain)
	vcl.Application.MainForm().SetDoubleBuffered(true)
	vcl.Application.Run()
}
