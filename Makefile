image_name = mnet-aggregator
all:
	@rm -rf ./build/*
	DOCKER_BUILDKIT=1 docker build --file Lambda.Dockerfile -t $(image_name) .
	id=$$(docker create $(image_name)); docker cp $$id:/root/output ./build; docker rm -v $$id
	cd build/output; zip -r function.zip *