#include "connectdiscordaccountform.h"
#include "ui_connectdiscordaccountform.h"

ConnectDiscordAccountForm::ConnectDiscordAccountForm(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ConnectDiscordAccountForm)
{
    ui->setupUi(this);
}

ConnectDiscordAccountForm::~ConnectDiscordAccountForm()
{
    delete ui;
}
