yval=fopen('Xd_values.txt');
y=textscan(yval,'%f %f %f %f');
Y=y{1,1};
L=length(Y)/4;
X=(1:1:L)';
figure(1);
plot(X,Y(1:L));
% title('Error vs Iterations')
% xlabel('Number of Iterations');
% ylabel('Error in X values');
% ylim([-0.05,0.8])

yval=fopen('Xb_values.txt');
y=textscan(yval,'%f %f %f %f');
Y=y{1,1};
X=(1:1:L)';
X=fliplr(X);
figure(2);
plot(X,Y);