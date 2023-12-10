package dockertest_test

import (
	"database/sql"
	"fmt"
	"log"
	"os"
	"testing"

	_ "github.com/denisenkom/go-mssqldb"
	"github.com/ory/dockertest/v3"
	"github.com/ory/dockertest/v3/docker"
)

var db *sql.DB

func TestMain(m *testing.M) {
	// uses a sensible default on windows (tcp/http) and linux/osx (socket)
	pool, err := dockertest.NewPool("")
	if err != nil {
		log.Fatalf("Could not construct pool: %s", err)
	}

	// uses pool to try to connect to Docker
	err = pool.Client.Ping()
	if err != nil {
		log.Fatalf("Could not connect to Docker: %s", err)
	}

	// pulls an image, creates a container based on it and runs it
	resource, err := pool.RunWithOptions(&dockertest.RunOptions{
		Repository: "mcr.microsoft.com/mssql/server",
		Tag:        "2019-latest",
		Env: []string{
			"ACCEPT_EULA=Y",
			"SA_PASSWORD=YourStrong@Passw0rd",
		},
		PortBindings: map[docker.Port][]docker.PortBinding{
			"1433/tcp": {{HostIP: "0.0.0.0", HostPort: "1433"}},
		},
	}, func(config *docker.HostConfig) {
		// 処理が終了したらインスタンスを削除する
		config.AutoRemove = true
		config.RestartPolicy = docker.RestartPolicy{
			Name: "no",
		}
	})
	if err != nil {
		log.Fatalf("Could not start resource: %s", err)
	}

	// exponential backoff-retry, because the application in the container might not be ready to accept connections yet
	if err = pool.Retry(func() error {
		var err error
		db, err = sql.Open("sqlserver", fmt.Sprintf("sqlserver://sa:YourStrong@Passw0rd@localhost:1433?database=master"))
		if err != nil {
			return err
		}
		return db.Ping()
	}); err != nil {
		log.Fatalf("Could not connect to docker: %s", err)
	}

	code := m.Run()

	// You can't defer this because os.Exit doesn't care for defer
	if err := pool.Purge(resource); err != nil {
		log.Fatalf("Could not purge resource: %s", err)
	}

	os.Exit(code)
}

func TestSomething(t *testing.T) {
	db.Ping()
}
