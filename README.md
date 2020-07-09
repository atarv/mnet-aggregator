# M.net listings aggregator

This application scrapes specified sections in [Muusikoiden.net
tori](https://muusikoiden.net/tori/), generates a report of new listings and
sends it to you via email. Ideally it is invoked recurringly with e.g. a cron
job. Now there's no reason to procrastinate with manually going through the
sections now and again!

Hacked together with: Haskell <sub>(+ various libraries)</sub> and Redis.

## Building

First clone the repository

```sh
git clone https://github.com/atarv/mnet-paivystaja
cd mnet-paivystaja
```

Then install
[Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) and
run

```sh
stack build
```

...or use Docker, but only after you have written the [configuration
file](#Configuration):

```sh
docker build -t mnet-aggregator .
```

Building _might_ take a while the first time.

## Configuration

Configuring is done by creating `config.dhall` file in repository root. For
more specific documentation take a look at [Configs.hs](/src/Configs.hs)
Configuration example:

```haskell
-- config.dhall
{ databaseConfig =
    { hostname = "www.redisdatabase.com"
    , databasePort = 6379
    , password = "auth"
    }
, mailConfig =
    { smtpHostname = "smtp.mailprovider.com"
    , senderEmail = "listings@yourdomain.com"
    , senderName = "Listings aggregator"
    , smtpPassword = "smtppassword"
    , smtpPort = 25
    , smtpUsername = "smtpuser"
    }
, serverPort = 8080
}
```

For setting up a Redis instance, see [their
documentation](https://redis.io/topics/quickstart) and for email
configuration you should look up your email service providers guides.

## Running

If you built the app with stack:

```sh
stack run
```

Docker:

```sh
docker run mnet-aggregator
```

## Usage

The aggregator is run, when a HTTP POST request is made to the
`/generatereport` path. Example with `curl`:

```sh
curl --request POST \
  --url http://localhost:8080/generatereport \
  --header 'content-type: application/json' \
  --data '{
    "recipientEmail": "your.email@domain.org",
    "recipientName": "Your name",
    "sections": [
        {
            "sectionTitle": "Guitars",
            "sectionUrl": "https://muusikoiden.net/tori/?category=8"
        },
        {
            "sectionTitle": "Amps",
            "sectionUrl": "https://muusikoiden.net/tori/?category=42"
        }
    ]
}'
```

You'll probably want to run this with a cron job. You can use URLs from
[Haku](https://muusikoiden.net/tori/haku.php), if you are looking for
specific gear.