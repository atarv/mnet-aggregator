# M.net listings aggregator

This application scrapes specified sections in [Muusikoiden.net
tori](https://muusikoiden.net/tori/), generates a report of new listings and
sends it to you via email. Ideally it is invoked recurringly with e.g. a cron
job. Now there's no reason to procrastinate with manually going through the
sections now and again!

Hacked together with Haskell <sub>(+ various libraries)</sub> and AWS.

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

### AWS Lambda

To get a zip-archive suitable for running on AWS Lambda run the Makefile at repository root.

```sh
make
```

To create the Lambda function see [AWS documentation](https://docs.aws.amazon.com/lambda/latest/dg/configuration-function-zip.html).
You can easily update the function by using AWS CLI:

```sh
aws lambda update-function-code --function-name example-function-name --zip-file fileb://build/output/function.zip
```

## Configuration

Configuring is done by creating `config.dhall` file in repository root. For
more specific documentation take a look at [Configs.hs](/src/Configs.hs)
Configuration example:

```haskell
-- config.dhall
{ mailConfig =
    { smtpHostname = "smtp.mailprovider.com"
    , senderEmail = "listings@yourdomain.com"
    , senderName = "Listings aggregator"
    , smtpPassword = "smtppassword"
    , smtpPort = 25
    , smtpUsername = "smtpuser"
    }
, serverPort = 8080
, dynamoDBTableName = "example-table"
}
```

For setting up a DynamoDB table see [AWS documentation](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/getting-started-step-1.html).
See `lambda/Main.hs` for which environment variables must be set.
Use `PK` as partition key and `SK` as sort key (both String type).

## Running

If you built the app with stack:

```sh
stack run
```

Docker:

```sh
docker run mnet-aggregator
```

Remember to set AWS credentials and AWS_REGION environment variables when running locally.

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