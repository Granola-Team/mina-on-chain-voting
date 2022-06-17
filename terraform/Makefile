up: infra config

init:
	@cd terraform && terraform init

infra: 
	@cd terraform && terraform apply --auto-approve

config: gen
	@ansible-playbook ansible/mina_new1.yaml -i ansible/inventory.ini

gen:
	./scripts/gen.sh

down:
	@cd terraform && terraform destroy --auto-approve
