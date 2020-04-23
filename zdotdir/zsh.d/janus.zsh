#!/bin/bash

ec2_for_janus() {
    JANUS_DEPLOYMENT_ID=$(bamc janus list-deployments -l 1000 --output json \
        | jq -r '.deployments[] | "\(.id) \(.application.name)"' \
        | sort -k2 -u | fzf | awk '{print $1}')

    read CLUSTER SERVICE_NAME REGION <<< $(bamc janus deployment-resource-set --output json --deployment-id $JANUS_DEPLOYMENT_ID \
        | jq -r '. | "\(.clusterName) \(.serviceName) \(.region)"')

    if [[ $CLUSTER == *"-dev-"* ]]; then
        profile=bamtech-test
    else
        profile=bamtech-prod
    fi

    tasklist=$(aw
                   --cluster $CLUSTER \
                   --output text \
                   --service-name $SERVICE_NAME | awk '{print $2}')

    echo "TASK LIST\n$tasklist"
    if [ -x $tasklist ]; then
        echo "no service ${SERVICE_NAME} found in ${CLUSTER} in ${REGION}"
        return
    fi

    containerlist=$(aws --region $REGION ecs describe-tasks \
                    --cluster $CLUSTER \
                    --output text \
                    --query 'tasks[].containerInstanceArn' \
                    --tasks $(echo $tasklist))

    echo "CONTAINER INSTANCES\n$containerlist"
    if [ -x $containerlist ]; then
        echo "no deployed tasks for ${SERVICE_NAME} found in ${CLUSTER} in ${REGION}"
        return
    fi

    ec2list=$(aws --region $REGION ecs describe-container-instances \
                  --cluster $CLUSTER \
                  --query 'containerInstances[].ec2InstanceId' \
                  --output text \
                  --container-instances $(echo containerlist))

    echo "$ec2list"
    if [ -x $ec2list ]; then
        echo "no deployed tasks for ${SERVICE_NAME} found in ${CLUSTER} in ${REGION}"
        return
    fi

    echo $ec2list | sed -e 's/[ \t]\+/\n/g'
}

alias janus-app-ec2='echo $(ec2_for_janus)'
