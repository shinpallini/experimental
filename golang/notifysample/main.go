package main

import (
	"log"

	"github.com/fsnotify/fsnotify"
)

func main() {
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		log.Fatal(err)
	}
	defer watcher.Close()

	go func() {
		for {
			select {
			case event, ok := <-watcher.Events:
				if !ok {
					log.Println("watcher.Events is not ok")
					return
				}

				// イベントをログに出力
				log.Printf("%+v", event)

				// if event.Has(fsnotify.Write) {
				// 	log.Println("modified file:", event.Name)
				// }s

			case err, ok := <-watcher.Errors:
				if !ok {
					log.Println("watcher.Errors is not ok")
					return
				}
				log.Println("error:", err)
			}
		}
	}()

	err = watcher.Add(".")
	if err != nil {
		log.Fatal(err)
	}
	<-make(chan struct{})
}
