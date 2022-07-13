# switch aws profile
awsp() {
    export AWS_PROFILE=$(grep profile ${HOME}/.aws/config \
        | awk '{print $2}' | sed 's,],,g' \
        | fzf --layout reverse --height=10% --border)
}

# copy aws account number
awsa() {
    grep -B1 --no-group-separator iam ${HOME}/.aws/credentials \
        | cut -d: -f5 | tr ']\n' ' ' | tr '[' '\n' \
        | column -t | fzf --height 10% --border \
        | tee /dev/tty | awk '{print $2}' \
        | xclip -selection clipboard
}

aws-sso() {
    selected_profile=$(grep profile ${HOME}/.aws/config \
        | awk '{print $2}' | sed 's,],,g' \
        | fzf --layout reverse --height=10% --border)
    aws sso login --profile ${selected_profile}
}

# SSH to instance via instance ID
sshi() {
    ssh ec2-user@$(aws ec2 describe-instances --instance-ids ${1:-$INSTANCEID} --query 'Reservations[].Instances[].NetworkInterfaces[].PrivateIpAddress' --output text)
}
# Start session manager to instance via ID
ssmi() {
    aws ssm start-session --target ${1:-INSTANCEID}
}
# Start session manager to instance via private IP
ssmip() {
    aws ssm start-session --target $(aws ec2 describe-instances --filter Name=private-ip-address,Values=${1} --query 'Reservations[].Instances[].[InstanceId]' --output text)
}
# Give me a dev vm in aws
# exports $INSTANCEID for ssmi and sshi functions
alias aws-devm='eval $(create-instance)'

# Mark instance unhealthy
alias aws-unhealthy='aws autoscaling set-instance-health --health-status Unhealthy --instance-id '
