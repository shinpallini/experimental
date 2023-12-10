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
	"github.com/stretchr/testify/require"
)

var db *sql.DB

func TestMain(m *testing.M) {
	var cleanup func()
	var err error
	db, cleanup, err = setupDatabase()
	if err != nil {
		log.Fatal(err)
	}
	defer cleanup()

	code := m.Run()
	os.Exit(code)
}

func setupDatabase() (*sql.DB, func(), error) {
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
		createTableSQL := `
		CREATE TABLE dbo.Users (
			ID int PRIMARY KEY,
			Name nvarchar(100),
			Email nvarchar(100)
		);`
		_, err = db.Exec(createTableSQL)
		if err != nil {
			return err
		}
		return db.Ping()
	}); err != nil {
		log.Fatalf("Could not connect to docker: %s", err)
	}

	// リソースのクリーンアップ関数
	cleanup := func() {
		if err := pool.Purge(resource); err != nil {
			log.Printf("Could not purge resource: %v", err)
		}
	}

	return db, cleanup, nil
}

func TestSomething(t *testing.T) {
	var tableName string
	err := db.QueryRow("SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = 'Users'").Scan(&tableName)

	require.NoError(t, err)
	t.Log("tableName: ", tableName)
	require.Equal(t, "Users", tableName, "Table dbo.Users does not exist")
}
